{-# LANGUAGE OverloadedStrings #-}

module Logic (
    InitOpts(..),
    Admins(..),
    process
) where

import Update (Stack, popUpdate, putError)
import Connection (Token, Photo2Send(..), Msg2Send(..), getFile, sendMessage, sendPhoto, downloadFile)

import Data.Text (Text, pack, unpack, take, drop, length, takeWhile)
import Data.List (singleton)
import qualified Data.Vector as Vector
import qualified Data.HashMap as HashMap
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import qualified Control.Monad.Extra as ME
import Control.Concurrent (threadDelay, forkIO)
import Network.Wreq
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Process as P
import System.IO as IO

data Admins = Admins { adminsId :: MVar [Int], adminsName :: [Text] }

type Photos = MVar (HashMap.Map Int [Text])

type Lock = MVar Int

lock :: Lock -> IO ()
lock mvar = do 
            _ <- takeMVar mvar
            return ()

unlock :: Lock -> IO ()
unlock mvar = putMVar mvar 0

wait :: Lock -> IO ()
wait mvar = do
            _ <- takeMVar mvar
            putMVar mvar 0

data State = State  {   stateToken :: Token
                    ,   stateScript :: Text
                    ,   stateInput :: Text
                    ,   stateOutput :: Text
                    ,   statePassword :: Text
                    ,   stateWaitingForPhoto :: Int
                    ,   stateAdmins :: Admins
                    ,   statePhotos :: Photos
                    ,   stateComputeLock :: Lock
                    ,   stateDownloadingLock :: Lock
                    }

data InitOpts = InitOpts    {   initStack :: Stack
                            ,   initToken :: Token
                            ,   initScript :: Text
                            ,   initInput :: Text
                            ,   initOutput :: Text
                            ,   initPassword :: Text
                            ,   initWaitingForPhoto :: Int
                            ,   initAdminsNames :: [Text]
                            ,   initAdminsIds :: [Int]
                            }

process :: InitOpts -> IO (MVar [Int])
process InitOpts    { initStack = stack
                    , initToken = token
                    , initScript = script
                    , initInput = input
                    , initOutput = output
                    , initPassword = password
                    , initWaitingForPhoto = waitingForPhoto
                    , initAdminsNames = adminsNames 
                    , initAdminsIds = adminsIds}  
                    = do
                    adminsIdsMVar <- newMVar adminsIds
                    computeLock <- newMVar 0
                    downloadLock <- newMVar 0
                    photos <- newMVar HashMap.empty
                    let admins = Admins adminsIdsMVar adminsNames
                    let state = State token script input output password waitingForPhoto admins photos computeLock downloadLock
                    _ <- forkIO $ processLoop stack state
                    return adminsIdsMVar

processLoop :: Stack -> State -> IO ()
processLoop stack state = do
                    update <- popUpdate stack
                    case update of
                        Nothing -> do
                                    threadDelay 1000000
                                    processLoop stack state
                        Just val -> do
                                    _ <- forkIO $ processMessage stack state val
                                    processLoop stack state

processMessage :: Stack -> State -> Value -> IO ()
processMessage stack state message = do
    case message ^? key "message" of
        (Just msg) ->
            case msg ^? key "text" . _String of
                (Just text) -> processTextMessage stack state text msg
                _ -> case msg ^? key "caption" . _String of
                    (Just text) -> processTextMessage stack state text msg
                    _ -> case getPhotoId msg of
                        Just (chatId, photoId) -> do
                            photos <- takeMVar (statePhotos state)
                            putMVar (statePhotos state) (HashMap.insertWith (++) chatId [photoId] photos)
                            _ <- forkIO $ forgetPhoto state (chatId, photoId)
                            return ()
                        _ -> do
                            response <- formQuestionResponse message
                            result <- sendMessage token response
                            case result of
                                Right _ -> return ()
                                Left e -> putError stack e
        _ -> return ()
    where
        token = stateToken state

forgetPhoto :: State -> (Int, Text) -> IO ()
forgetPhoto state (chatId, photoId) = do
    threadDelay (stateWaitingForPhoto state * 2000000)
    photos <- takeMVar (statePhotos state)
    putMVar (statePhotos state) (HashMap.update (removePhoto photoId) chatId photos)
    return ()
    where
        removePhoto p photoIds = Just (filter (/= p) photoIds)

processTextMessage :: Stack -> State -> Text -> Value -> IO ()
processTextMessage stack state text msg
    | match "!" = return ()
    | match "/ping" = do
        response <- formEchoResponse text msg
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    | login = do
        appendAdmin admins msg
        response <- formTextResponse msg "Logged in!"
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    | match "/sys " = adminCommand $ do
        response <- sysResponse (Data.Text.drop 5 text) msg
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    | match "/" = adminCommand $ do
        preparationResponse <- formTextResponse msg "Added to queue."
        preparationResult <- sendMessage token preparationResponse
        case preparationResult of
            Right _ -> do
                    threadDelay (waitingForPhoto * 1000000)
                    lock computeLock
                    processingResponse <- formTextResponse msg "Processing..."
                    processingResult <- sendMessage token processingResponse
                    case processingResult of
                        Right _ ->  do
                                    let photoId = getPhotoId msg
                                    photo <- downloadPhoto token stack input photoId downloadLock
                                    case photo of
                                        Nothing -> do
                                                response <- formPythonResponse msg script output (Data.Text.drop 1 text) []
                                                unlock computeLock
                                                result <- sendPhoto token response
                                                case result of
                                                    Right _ -> return ()
                                                    Left e -> putError stack e
                                        Just photoPath -> do
                                                prevPhotos <- getPhotos token stack input photos chatId downloadLock
                                                wait downloadLock
                                                let photosPaths = prevPhotos ++ [photoPath]
                                                response <- formPythonResponse msg script output (Data.Text.drop 1 text) photosPaths
                                                unlock computeLock
                                                result <- sendPhoto token response
                                                case result of
                                                    Right _ -> return ()
                                                    Left e -> putError stack e
                        Left e -> do
                                    unlock computeLock
                                    putError stack e

            Left e -> putError stack e
    | otherwise = adminCommand $ do
        response <- formQuestionResponse msg
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    where
        token = stateToken state
        admins = stateAdmins state
        login = text == "/login " <> statePassword state
        match c = Data.Text.take (Data.Text.length c) text == c
        photos = statePhotos state
        input = stateInput state
        output = stateOutput state
        script = stateScript state
        computeLock = stateComputeLock state
        downloadLock = stateDownloadingLock state
        waitingForPhoto = stateWaitingForPhoto state
        chatId = msg ^?! key "chat" . key "id" . _Integral
        adminCommand command = do
            isAdmin <- checkIfIsAdmin admins msg
            if isAdmin
                then command
                else do
                    response <- formTextResponse msg "Not logged in!"
                    result <- sendMessage token response
                    case result of
                        Right _ -> return ()
                        Left e -> putError stack e

appendAdmin :: Admins -> Value -> IO ()
appendAdmin admins msg = do 
                    adminsIdsMVar <- takeMVar (adminsId admins)
                    putMVar (adminsId admins) (appendId adminsIdsMVar)

    where   appendId adminsIds = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> adminsIds ++ [adminId | adminId `notElem` adminsIds]
                            Nothing -> adminsIds

checkIfIsAdmin :: Admins -> Value -> IO Bool
checkIfIsAdmin admins msg = do
                    adminsIds <- readMVar (adminsId admins)
                    let idMatch = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> adminId `elem` adminsIds
                            Nothing -> False
                    return (idMatch || nameMatch)
                    where
                        nameMatch = case senderLogin msg of
                            Just name -> name `elem` adminsName admins
                            Nothing -> False

senderLogin :: Value -> Maybe Text
senderLogin msg = msg ^? key "from" . key "username" . _String

getPhotoId :: Value -> Maybe (Int, Text)
getPhotoId msg = addSenderId =<< (handlePhotos =<< (msg ^? key "photo"))
                 where addSenderId photoId = Just ( msg ^?! key "chat" . key "id" . _Integral
                                                  , photoId)

getPhotos :: Token -> Stack -> Text -> Photos -> Int -> Lock -> IO [String]
getPhotos token stack downloadDir photosMVar chatId downloadLock = do 
                                                photos <- takeMVar photosMVar
                                                let newPhotos = HashMap.delete chatId photos
                                                putMVar photosMVar newPhotos
                                                case HashMap.lookup chatId photos of
                                                    Just photoIds -> do
                                                        photosPaths <- mapM downloadSingle photoIds
                                                        return $ concat photosPaths
                                                    Nothing -> return [] 
                                                    where
                                                        downloadSingle photoId = 
                                                            ME.maybeM (return []) (return . Data.List.singleton) $
                                                            downloadPhoto token stack downloadDir (Just (chatId, photoId)) downloadLock

handlePhotos :: Value -> Maybe Text
handlePhotos (Array photo) = Just $ handlePhoto (Vector.last photo)
                             where
                                handlePhoto p = p ^?! key "file_id" . _String
handlePhotos _ = Nothing

downloadPhoto :: Token -> Stack -> Text -> Maybe (Int, Text) -> Lock -> IO (Maybe String)
downloadPhoto token stack downloadDir maybePhotoId downloadLock = do
                    case maybePhotoId of
                        Nothing -> return Nothing
                        Just (_, photoId) -> do
                            getFileAttempt <- getFile token photoId
                            case getFileAttempt of
                                Right responseFile -> do
                                    posixTime <- getPOSIXTime
                                    let fileId = responseFile ^?! responseBody ^?! key "result" . key "file_path" . _String
                                    let downloadPhotoUrl = downloadFile token (unpack fileId)
                                    let pathToPhoto = show posixTime ++ ".jpg"
                                    _ <- forkIO $ execWget pathToPhoto downloadPhotoUrl
                                    return $ Just pathToPhoto
                                Left e -> do
                                    putError stack e
                                    return Nothing
                    where execWget p url = do
                            (_, Just hout, _, ph) <- P.createProcess (proc "wget" ["-O", unpack downloadDir ++ "/" ++ p, url]) 
                                { std_out = P.CreatePipe }
                            lock downloadLock
                            _ <- P.waitForProcess ph
                            cmdline <- IO.hGetContents hout
                            putStrLn cmdline
                            unlock downloadLock


formPythonResponse :: Value -> Text -> Text -> Text -> [String] -> IO Photo2Send
formPythonResponse msg script output text photos = do
                    let command = Data.Text.takeWhile (/= ' ') text
                    let prompt = Data.Text.drop (Data.Text.length command + 1) text
                    pythonResult <- execPython script command output prompt photos
                    return $ Photo2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    pythonResult

formTextResponse :: Value -> Text -> IO Msg2Send
formTextResponse msg text = do
                    return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    text


sysResponse :: Text -> Value -> IO Msg2Send
sysResponse text msg = do
                    (_, Just hout, _, ph) <- P.createProcess (proc "zsh" ["-c", unpack text]) { std_out = P.CreatePipe }
                    _ <- P.waitForProcess ph
                    cmdline <- IO.hGetContents hout
                    putStrLn cmdline
                    formTextResponse msg (pack cmdline)

formEchoResponse :: Text -> Value -> IO Msg2Send
formEchoResponse text msg = do
                    let msgText = Data.Text.drop 6 text
                    if msgText == ""
                        then return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "Pong!"
                        else return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    msgText

formQuestionResponse :: Value -> IO Msg2Send
formQuestionResponse msg = do
                    return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "???"

execPython :: Text -> Text -> Text -> Text -> [String] -> IO String
execPython path command output prompt photos = do
    let promptOpt = case prompt of
                "" -> []
                _ -> ["-p", prompt]
    let photoOpt = concatMap (\p -> ["-i", "/home/damakm/TelegramImageAiBot/downloads/" ++ p]) photos
    posixTime <- getPOSIXTime
    let outputFile = "/home/damakm/TelegramImageAiBot/results" ++ "/" ++ show posixTime ++ ".png"
    let opts = map unpack ([path, command] ++ promptOpt) ++ photoOpt
    let envVars = [("IMAGINE_OUTPUT", outputFile)]
    putStrLn $ show envVars ++ show ("python3" : opts)
    (_, _, _, phCp) <- P.createProcess (proc "cp" ["../err.jpg", outputFile]) { std_out = P.CreatePipe }
    _ <- P.waitForProcess phCp
    (_, Just hout, _, ph) <- P.createProcess (proc "python3" opts) { std_out = P.CreatePipe, env = Just envVars}
    _ <- P.waitForProcess ph
    cmdline <- IO.hGetContents hout
    putStrLn cmdline
    return outputFile

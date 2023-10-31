{-# LANGUAGE OverloadedStrings #-}

module Logic (
    InitOpts(..),
    Admins(..),
    process
) where

import Update (Stack, popUpdate, putError)
import Connection (Token, Photo2Send(..), Msg2Send(..), getFile, sendMessage, sendPhoto, downloadFile)

import Data.Text (Text, pack, unpack, take, drop, length, takeWhile)
import qualified Data.Vector as Vector
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
import Network.Wreq
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Process as P
import System.IO as IO

data Admins = Admins { adminsId :: MVar [Int], adminsName :: [Text] }

data State = State  {   stateToken :: Token
                    ,   stateScript :: Text
                    ,   stateInput :: Text
                    ,   stateOutput :: Text
                    ,   statePassword :: Text
                    ,   stateAdmins :: Admins
                    }

data InitOpts = InitOpts    {   initStack :: Stack
                            ,   initToken :: Token
                            ,   initScript :: Text
                            ,   initInput :: Text
                            ,   initOutput :: Text
                            ,   initPassword :: Text
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
                    , initAdminsNames = adminsNames 
                    , initAdminsIds = adminsIds}  
                    = do
                    adminsIdsMVar <- newMVar adminsIds
                    let admins = Admins adminsIdsMVar adminsNames
                    let state = State token script input output password admins
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
                    _ -> do
                        response <- formQuestionResponse msg
                        result <- sendMessage token response
                        case result of
                            Right _ -> return ()
                            Left e -> putError stack e
        _ -> return ()
    where
        token = stateToken state

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
        response <- formLoginResponse msg
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
        photo <- getPhoto token stack (stateInput state) msg
        response <- formPythonResponse msg (stateScript state) (stateOutput state) (Data.Text.drop 1 text) photo
        result <- sendPhoto token response
        case result of
            Right _ -> return ()
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
        adminCommand command = do
            isAdmin <- checkIfIsAdmin admins msg
            if isAdmin
                then command
                else do
                    response <- formNotLoggedInResponse msg
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

getPhoto :: Token -> Stack -> Text -> Value -> IO (Maybe String)
getPhoto token stack downloadDir msg = do
                let photoId = handlePhotos =<<(msg ^? key "photo")
                case photoId of
                    Nothing -> return Nothing
                    Just p -> downloadPhoto token stack downloadDir p

handlePhotos :: Value -> Maybe Text
handlePhotos (Array photo) = Just $ handlePhoto (Vector.last photo)
                             where
                                handlePhoto p = p ^?! key "file_id" . _String
handlePhotos _ = Nothing

downloadPhoto :: Token -> Stack -> Text -> Text -> IO (Maybe String)
downloadPhoto token stack downloadDir photoId = do
                    getFileAttempt <- getFile token photoId
                    case getFileAttempt of
                        Right responseFile -> do
                            posixTime <- getPOSIXTime
                            let fileId = responseFile ^?! responseBody ^?! key "result" . key "file_path" . _String
                            let downloadPhotoUrl = downloadFile token (unpack fileId)
                            let pathToPhoto = show posixTime ++ ".jpg"
                            (_, Just hout, _, ph) <- P.createProcess (proc "wget" [downloadPhotoUrl, "-o", unpack downloadDir ++ "/" ++ pathToPhoto]) 
                                { std_out = P.CreatePipe }
                            _ <- P.waitForProcess ph
                            cmdline <- IO.hGetContents hout
                            putStrLn cmdline
                            return $ Just pathToPhoto
                        Left e -> do
                            putError stack e
                            return Nothing

formPythonResponse :: Value -> Text -> Text -> Text -> Maybe String -> IO Photo2Send
formPythonResponse msg script output text photo = do
                    let command = Data.Text.takeWhile (/= ' ') text
                    let prompt = Data.Text.drop (Data.Text.length command + 1) text
                    pythonResult <- execPython script command output prompt photo
                    return $ Photo2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    pythonResult

sysResponse :: Text -> Value -> IO Msg2Send
--run bash command
sysResponse text msg = do
                    (_, Just hout, _, ph) <- P.createProcess (proc "zsh" ["-c", unpack text]) { std_out = P.CreatePipe }
                    _ <- P.waitForProcess ph
                    cmdline <- IO.hGetContents hout
                    putStrLn cmdline
                    return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    (pack cmdline)


formLoginResponse :: Value -> IO Msg2Send
formLoginResponse msg = do
                    return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "Logged in!"

formNotLoggedInResponse :: Value -> IO Msg2Send
formNotLoggedInResponse msg = do
                    return $ Msg2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    "Not logged in!"

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

execPython :: Text -> Text -> Text -> Text -> Maybe String -> IO String
execPython path command output prompt photo = do
    let promptOpt = ["-p", prompt]
    let photoOpt = case photo of
                Just p -> ["-i", "/home/damakm/TelegramImageAiBot/downloads/" ++ p]
                Nothing -> []
    posixTime <- getPOSIXTime
    let outputFile = unpack output ++ "/" ++ show posixTime ++ ".png"
    let opts = map unpack ([path, command] ++ promptOpt) ++ photoOpt
    let envVars = Just [("IMAGINE_OUTPUT", outputFile)]
    putStrLn $ show envVars ++ show ("python3" : opts)
    (_, Just hout, _, ph) <- P.createProcess (proc "python3" opts) { std_out = P.CreatePipe, env = envVars}
    _ <- P.waitForProcess ph
    cmdline <- IO.hGetContents hout
    putStrLn cmdline
    return outputFile

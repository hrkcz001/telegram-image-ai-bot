{-# LANGUAGE OverloadedStrings #-}

module Logic (
    InitOpts(..),
    Admins(..),
    process
) where

import Update (Stack, popUpdate, putError)
import Connection (Token, Photo2Send(..), Msg2Send(..), sendMessage, sendPhoto)

import Data.Text (Text, unpack, take, drop, length)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Process as P
import System.IO as IO

data Admins = Admins { adminsId :: [Int], adminsName :: [Text] }

data State = State  {   stateToken :: Token
                    ,   stateScript :: Text
                    ,   stateOutput :: Text
                    ,   statePassword :: Text
                    ,   stateAdmins :: MVar Admins
                    }

data InitOpts = InitOpts    {   initStack :: Stack
                            ,   initToken :: Token
                            ,   initScript :: Text
                            ,   initOutput :: Text
                            ,   initPassword :: Text
                            ,   initAdminsNames :: [Text]
                            ,   initAdminsIds :: [Int]
                            }

process :: InitOpts -> IO (MVar Admins)
process InitOpts    { initStack = stack
                    , initToken = token
                    , initScript = script
                    , initOutput = output
                    , initPassword = password
                    , initAdminsNames = adminsNames 
                    , initAdminsIds = adminsIds}  
                    = do
                    adminsMVar <- newMVar $ Admins adminsIds adminsNames
                    let state = State token script output password adminsMVar
                    _ <- forkIO $ processLoop stack state
                    return adminsMVar

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
    | text `match` "!" = return ()
    | text `match` "/ping" = do
        response <- formEchoResponse text msg
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    | login text = do
        appendAdmin (stateAdmins state) msg
        response <- formLoginResponse msg
        result <- sendMessage token response
        case result of
            Right _ -> return ()
            Left e -> putError stack e
    | otherwise = do
        admins <- readMVar $ stateAdmins state
        if isAdmin admins msg
            then do
                response <- formResponse msg (stateScript state) (stateOutput state) text
                result <- sendPhoto token response
                case result of
                    Right _ -> return ()
                    Left e -> putError stack e
            else do
                response <- formNotLoggedInResponse msg
                result <- sendMessage token response
                case result of
                    Right _ -> return ()
                    Left e -> putError stack e
    where
        token = stateToken state
        login t = t == "/login " <> statePassword state
        match t c = Data.Text.take (Data.Text.length c) t == c

appendAdmin :: MVar Admins -> Value -> IO ()
appendAdmin adminsMVar msg = do 
                    admins <- takeMVar adminsMVar
                    let newAdmins = Admins (appendId admins) (appendName admins)
                    putMVar adminsMVar newAdmins

    where   appendId admins = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> adminsId admins ++ [adminId | adminId `notElem` adminsId admins]
                            Nothing -> adminsId admins
            appendName admins = case senderLogin msg of
                            Just name -> adminsName admins ++ [name | name `notElem` adminsName admins]
                            Nothing -> adminsName admins

isAdmin :: Admins -> Value -> Bool
isAdmin admins msg = idMatch || nameMatch 
    where   idMatch   = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> adminId `elem` adminsId admins
                            Nothing -> False
            nameMatch = case senderLogin msg of
                            Just name -> name `elem` adminsName admins
                            Nothing -> False

senderLogin :: Value -> Maybe Text
senderLogin msg = msg ^? key "from" . key "username" . _String

formResponse :: Value -> Text -> Text -> Text -> IO Photo2Send
formResponse msg script output prompt = do
                    pythonResult <- execPython script output prompt
                    return $ Photo2Send
                                    (msg ^?! key "chat" . key "id" . _Integral)
                                    (Just (msg ^?! key "message_id" . _Integral))
                                    pythonResult

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

execPython :: Text -> Text -> Text -> IO String
execPython path output prompt = do
    posixTime <- getPOSIXTime
    let outputFile = unpack output ++ show posixTime
    let opts = [unpack path, outputFile , unpack prompt]
    (_, Just hout, _, ph) <- P.createProcess (proc "python3" opts) { std_out = P.CreatePipe }
    _ <- P.waitForProcess ph
    cmdline <- IO.hGetContents hout
    putStrLn cmdline
    return outputFile

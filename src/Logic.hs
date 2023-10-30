{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Logic (
    InitOpts(..),
    process
) where

import Update (Stack, popUpdate, putError)
import Connection (Token, Photo2Send(..), Msg2Send(..), sendMessage, sendPhoto)

import Data.Text (Text, unpack, take, drop)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens hiding ((.=))
import Control.Concurrent (threadDelay, forkIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

import System.Process as P
import System.IO as IO

data Admins = Admins { adminsId :: [Int], adminsName :: [Text] }

data State = State  {   stateToken :: Token
                    ,   stateScript :: Text
                    ,   stateOutput :: Text
                    ,   statePassword :: Text
                    ,   stateAdmins :: Admins
                    }

data InitOpts = InitOpts    {   initStack :: Stack
                            ,   initToken :: Token
                            ,   initScript :: Text
                            ,   initOutput :: Text
                            ,   initPassword :: Text
                            ,   initAdmins :: [Text]
                            }

initState :: IO State
initState = do
                return $ State "" "" "" "" ( Admins [] [] )

process :: InitOpts -> IO ()
process InitOpts    { initStack = stack
                    , initToken = token
                    , initScript = script
                    , initOutput = output
                    , initPassword = password
                    , initAdmins = admins }  
                    = do
                    emptyState <- initState
                    let state = emptyState  { stateToken = token 
                                            , stateScript = script
                                            , stateOutput = output
                                            , statePassword = password
                                            , stateAdmins = Admins [] admins }
                    _ <- forkIO $ processLoop stack state
                    return ()

processLoop :: Stack -> State -> IO ()
processLoop stack state = do
                    update <- popUpdate stack
                    case update of
                        Nothing -> do
                                    threadDelay 1000000
                                    processLoop stack state
                        Just val -> do
                                    newState <- processMessage stack state val
                                    processLoop stack newState

processMessage :: Stack -> State -> Value -> IO State
processMessage stack state message = do
    case message ^? key "message" of
        Just msg -> case msg ^? key "text" . _String of
                        Just text -> case text of
                                        (first 1 -> "!") -> return state
                                        (first 5 -> "/ping") -> do
                                                            response <- formEchoResponse msg
                                                            result <- sendMessage token $ response
                                                            case result of
                                                                Right _ -> return state
                                                                Left e -> do
                                                                        putError stack e
                                                                        return state
                                        (login -> True) -> do
                                                            response <- formLoginResponse msg 
                                                            result <- sendMessage token $ response
                                                            case result of
                                                                Right _ -> return $ state { stateAdmins = appendAdmin admins msg }
                                                                Left e -> do
                                                                        putError stack e
                                                                        return state
                                        _ ->
                                                if isAdmin admins msg
                                                    then
                                                        do
                                                            response <- formResponse msg 
                                                                                     (stateScript state) 
                                                                                     (stateOutput state)
                                                                                     text
                                                            result <- sendPhoto token $ response
                                                            case result of
                                                                Right _ -> return state
                                                                Left e -> do
                                                                        putError stack e
                                                                        return state
                                                    else do
                                                            response <- formNotLoggedInResponse msg
                                                            result <- sendMessage token $ response
                                                            case result of
                                                                Right _ -> return state
                                                                Left e -> do
                                                                        putError stack e
                                                                        return state
                        _ -> do
                                response <- formQuestionResponse msg
                                result <- sendMessage token $ response
                                case result of
                                    Right _ -> return state
                                    Left e -> do
                                            putError stack e
                                            return state
                        where 
                            token = stateToken state
                            passwd = statePassword state
                            admins = stateAdmins state
                            first n = unpack . Data.Text.take n
                            login = (==) $ "/login " <> passwd
        _ -> return state

appendAdmin :: Admins -> Value -> Admins
appendAdmin admins msg = if isAdmin admins msg
                         then    admins
                         else    Admins 
                                 (adminsId admins ++ appendId)
                                 (adminsName admins ++ appendName)
    where   appendId = case msg ^? key "chat" . key "id" . _Integral of
                            Just adminId -> [adminId | adminId `notElem` adminsId admins]
                            Nothing -> []
            appendName = case senderLogin msg of
                            Just name -> [name | name `notElem` adminsName admins]
                            Nothing -> []

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

formEchoResponse :: Value -> IO Msg2Send
formEchoResponse msg = do
                    let msgText = Data.Text.drop 6 (msg ^?! key "text" . _String)
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
    let outputFile = (unpack output) ++ (show posixTime)
    let opts = [unpack path, outputFile , unpack prompt]
    (_, Just hout, _, ph) <- P.createProcess (proc "python3" opts) { std_out = P.CreatePipe }
    _ <- P.waitForProcess ph
    cmdline <- IO.hGetContents hout
    putStrLn cmdline
    return outputFile

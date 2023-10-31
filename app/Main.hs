{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token)
import Configuration (Config(..), readConfig, updateConfigLoop)
import Update (InitOpts(..), Stack, init, popError)
import Logic (InitOpts(..), process)
import Control.Concurrent (threadDelay)
import Data.Text (Text, pack)
import Control.Concurrent.MVar
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        let configPath = if not (null args) then head args else "config.json"
        readConfigAttempt <- readConfig configPath
        case readConfigAttempt of
            Nothing -> return ()
            Just config -> do
                        stack <- initBot token updateTimeout
                        admins <- initLogic token stack script input output password waitingFotPhoto adminsNames adminsIds
                        _ <- updateConfigLoop configPath delay config admins
                        errorLoop stack
                        where 
                            delay = 5  -- delay between config updates in seconds
                            token = configToken config
                            updateTimeout = configTimeout config 
                            script = pack $ configScript config
                            input = pack $ configInput config
                            output = pack $ configOutput config
                            password = pack $ configPassword config
                            waitingForPhoto = configWaitingForPhoto config
                            adminsNames = map pack $ configAdminsNames config
                            adminsIds = configAdminsIds config

initBot :: Token -> Int -> IO Stack
initBot token timeout = Update.init $ Update.InitOpts token timeout

initLogic :: Token -> Stack -> Text -> Text -> Text -> Text -> Int -> [Text] -> [Int] -> IO (MVar [Int])
initLogic token stack script input output password waitingForPhoto adminsNames adminsIds = 
    Logic.process $ Logic.InitOpts stack token script input output password waitingForPhoto updateTimeout adminsNames adminsIds

errorLoop :: Stack -> IO ()
errorLoop stack = do
                    err <- popError stack
                    case err of
                        Nothing  -> do
                                    threadDelay 1000000
                                    errorLoop stack
                        Just val -> do
                                    print val
                                    errorLoop stack

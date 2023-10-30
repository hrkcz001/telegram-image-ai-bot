{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token)
import Configuration (Config(..), readConfig, updateConfigLoop)
import Update (InitOpts(..), Stack, init, popError)
import Logic (InitOpts(..), Admins(..), process)
import Control.Concurrent (threadDelay)
import Data.Text (Text, pack)
import Control.Concurrent.MVar
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        let configPath = if length args > 0 then args !! 0 else "config.json"
        readConfigAttempt <- readConfig configPath
        case readConfigAttempt of
            Nothing -> return ()
            Just config -> do
                        stack <- initBot token updateTimeout
                        admins <- initLogic token stack script output password adminsNames adminsIds
                        _ <- updateConfigLoop configPath delay config admins
                        errorLoop stack
                        where 
                            delay = 5  -- delay between config updates in seconds
                            token = configToken config
                            updateTimeout = configTimeout config 
                            script = pack $ configScript config
                            output = pack $ configOutput config
                            password = pack $ configPassword config
                            adminsNames = map pack $ configAdminsNames config
                            adminsIds = configAdminsIds config

initBot :: Token -> Int -> IO Stack
initBot token timeout = Update.init $ Update.InitOpts token timeout

initLogic :: Token -> Stack -> Text -> Text -> Text -> [Text] -> [Int] -> IO (MVar Admins)
initLogic token stack script output password adminsNames adminsIds = 
    Logic.process $ Logic.InitOpts stack token script output password adminsNames adminsIds

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

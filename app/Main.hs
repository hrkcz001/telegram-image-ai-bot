{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Connection (Token)
import Update (InitOpts(..), Stack, init, popError)
import Logic (InitOpts(..), process)
import Control.Concurrent (threadDelay)
import Data.Text (Text)

main :: IO ()
main = do
        stack <- initBot token updateTimeout
        _ <- initLogic token stack script output password admins
        errorLoop stack
            where 
                token = ""
                updateTimeout = 5  -- timeout to get updates in seconds
                script = ""
                output = ""
                password = ""
                admins = [  
                         ]

initBot :: Token -> Int -> IO Stack
initBot token timeout = Update.init $ Update.InitOpts token timeout

initLogic :: Token -> Stack -> Text -> Text -> Text -> [Text] -> IO ()
initLogic token stack script output password admins = 
    Logic.process $ Logic.InitOpts stack token script output password admins

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

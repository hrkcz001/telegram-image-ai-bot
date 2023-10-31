{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Configuration (
    Config(..),
    readConfig,
    updateConfigLoop
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import GHC.Generics
import Control.Exception as E
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

data Config = Config
  { configToken :: String
  , configTimeout :: Int
  , configScript :: String
  , configInput :: String
  , configOutput :: String
  , configPassword :: String
  , configAdminsNames :: [String]
  , configAdminsIds :: [Int]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

defaultConfig :: Config
defaultConfig = Config { configToken = "put your token here"
                       , configTimeout = 5
                       , configScript = "put path to your python script here"
                       , configInput = "put path to your input folder here"
                       , configOutput = "put path to your output folder here"
                       , configPassword = "put bot password here"
                       , configAdminsNames = []
                       , configAdminsIds = []
                       }

isDefault :: Config -> Bool
isDefault config = config == defaultConfig

createConfig :: FilePath -> IO ()
createConfig filePath = do
                        putStrLn "Config file not found."
                        B.writeFile filePath (encode defaultConfig)
                        putStrLn "Default config written to file. Change it and restart the bot."

readConfig :: FilePath -> IO (Maybe Config)
readConfig filePath = do
    readFileAttempt <- E.try $ B.readFile filePath :: IO (Either IOError B.ByteString)
    case readFileAttempt of
      Left e -> do
            print e
            createConfig filePath
            return Nothing
      Right file -> do
            let config = decode file :: Maybe Config
            case config of
              Nothing -> do
                    createConfig filePath
                    return Nothing
              Just content -> if isDefault content
                                 then do
                                    putStrLn "Default config found. Change it and restart the bot."
                                    return Nothing 
                                 else do
                                    putStrLn "Config file read successfully."
                                    return config

updateConfigLoop :: FilePath -> Int -> Config -> MVar [Int] -> IO ()
updateConfigLoop filePath delay config adminsIdsMVar = do
    threadDelay $ delay * 1000000
    adminsIds <- readMVar adminsIdsMVar
    let newConfig = config { configAdminsIds = adminsIds }
    B.writeFile filePath (encode newConfig)
    updateConfigLoop filePath delay newConfig adminsIdsMVar

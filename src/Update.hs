{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Update (
    Update.init,
    InitOpts(..),
    Stack,
    popUpdate,
    popError,
    putError
) where

import Connection (Token, Error(..), getUpdate)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Control.Concurrent.MVar
import Data.Vector as Vector
import Data.ByteString.Lazy as ByteString
import Network.Wreq
import Control.Concurrent (forkIO, threadDelay)

data Stack = Stack { stackUpdates :: MVar [Value], stackErrors :: MVar [Error] }

putError :: Stack -> Error -> IO ()
putError stack err = do
                        curErrors <- takeMVar errors
                        putMVar errors $ curErrors Prelude.++ [err]
                        where errors = stackErrors stack

popError :: Stack -> IO (Maybe Error)
popError stack = do
                    curErrors <- takeMVar errors
                    case curErrors of
                        [] -> do
                                putMVar errors []
                                return Nothing
                        (x:xs) -> do
                                    putMVar errors xs
                                    return $ Just x
                    where errors = stackErrors stack

putUpdates :: Stack -> [Value] -> IO ()
putUpdates _ [] = return ()
putUpdates stack vals = do
                        curStack <- takeMVar updates
                        putMVar updates $ curStack Prelude.++ vals
                        where updates = stackUpdates stack

popUpdate :: Stack -> IO (Maybe Value)
popUpdate stack = do
                    curStack <- takeMVar updates
                    case curStack of
                        [] -> do
                                putMVar updates []
                                return Nothing
                        (x:xs) -> do
                                    putMVar updates xs
                                    return $ Just x
                    where updates = stackUpdates stack

data State = State { stateToken :: Token, stateTimeout :: Int, stateOffset :: Int}
data InitOpts = InitOpts { initToken :: Token , initTimeout :: Int}

init :: InitOpts -> IO Stack
init initOpts = do
                    newUpdates <- newMVar []
                    newErrors <- newMVar []
                    let newStack = Stack newUpdates newErrors
                    _ <- forkIO $ updateLoop newStack (State token timeout 0)
                    return newStack
                    where
                        token = initToken initOpts
                        timeout = initTimeout initOpts


data Update = Update { updateOffset :: Maybe Int, updateResult :: Either Error [Value] }

emptyUpdate :: Update
emptyUpdate = Update (Just 0) (Right [])

raiseError :: Error -> Update
raiseError e = emptyUpdate { updateResult = Left e }

updateLoop :: Stack -> State -> IO ()
updateLoop stack state = do
                        response <- getUpdate (stateToken state) (stateTimeout state) (stateOffset state)
                        newState <- case response of
                            Left  e -> putError stack e
                                       >> return state
                            Right r -> case handleResponse r of
                                        Update { updateOffset = offset 
                                               , updateResult = result }
                                                -> case result of
                                                        Left  e -> putError stack e
                                                                   >> return state
                                                        Right v -> putUpdates stack v
                                                                   >> case offset of
                                                                        Just o -> return state { stateOffset = o }
                                                                        _ -> return state
                        threadDelay $ stateTimeout newState * 1000000
                        updateLoop stack newState

handleResponse :: Response ByteString -> Update
handleResponse response = case response ^? responseStatus . statusCode of
                              Just 200 -> case response ^? responseBody of
                                              Just body -> handleBody body
                                              _ -> raiseError InvalidResponse
                              Just code -> raiseError $ StatusCode code
                              _ -> raiseError InvalidResponse

handleBody :: ByteString -> Update
handleBody body = case body ^? key "ok" of
    Just (Bool True) -> case body ^? key "result" of
                            Just result -> handleUpdate result
                            _ -> raiseError InvalidResponse
    Just (Bool False) -> case body ^? key "description" of
                            Just (String s) -> raiseError $ Other s
                            _ -> raiseError InvalidResponse
    _ -> raiseError InvalidResponse

handleUpdate :: Value -> Update
handleUpdate result = Update (handleOffset result) (handleResult result)

handleOffset :: Value -> Maybe Int
handleOffset (Array (Vector.null -> True)) = Nothing
handleOffset (Array vector) = case Vector.last vector ^? key "update_id" of
                          Just (Number n) -> Just $ floor n + 1
                          _ -> Nothing
handleOffset _ = Nothing

handleResult :: Value -> Either Error [Value]
handleResult (Array vector) = Right $ Vector.toList vector
handleResult _ = Left InvalidResponse

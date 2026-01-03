{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : The0.State
-- Description : Persistent state management for the0 trading bots
-- License     : Apache-2.0
--
-- This module provides persistent state management for bots across executions.
-- State is automatically synced to MinIO storage between bot runs.
--
-- = Example
--
-- @
-- import The0.State as State
-- import Data.Aeson (object, (.=))
--
-- main :: IO ()
-- main = do
--     -- Store state
--     State.set "portfolio" (object ["AAPL" .= (100 :: Int), "GOOGL" .= (50 :: Int)])
--
--     -- Retrieve state
--     portfolio <- State.get "portfolio"
--
--     -- List all keys
--     keys <- State.list
--
--     -- Delete a key
--     deleted <- State.delete "portfolio"
--
--     -- Clear all state
--     State.clear
-- @
module The0.State
    ( get
    , set
    , The0.State.delete
    , list
    , clear
    , exists
    ) where

import Control.Exception (catch, SomeException)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension, dropExtension)

-- | Get the path to the state directory.
getStateDir :: IO FilePath
getStateDir = fromMaybe "/state/.the0-state" <$> lookupEnv "STATE_DIR"

-- | Get the file path for a state key.
getKeyPath :: String -> IO FilePath
getKeyPath key = do
    stateDir <- getStateDir
    return $ stateDir </> (key ++ ".json")

-- | Validate that a key is safe to use as a filename.
validateKey :: String -> IO ()
validateKey key
    | null key = ioError $ userError "State key cannot be empty"
    | '/' `elem` key || '\\' `elem` key || ".." `isInfixOf` key =
        ioError $ userError "State key cannot contain path separators or '..'"
    | otherwise = return ()
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

-- | Get a value from persistent state.
--
-- Returns 'Nothing' if the key doesn't exist or deserialization fails.
--
-- @
-- portfolio <- State.get "portfolio" :: IO (Maybe Portfolio)
-- @
get :: FromJSON a => String -> IO (Maybe a)
get key = do
    validateKey key
    filepath <- getKeyPath key
    exists' <- doesFileExist filepath
    if not exists'
        then return Nothing
        else do
            content <- BL.readFile filepath `catch` handleError
            return $ decode content
  where
    handleError :: SomeException -> IO BL.ByteString
    handleError _ = return ""

-- | Set a value in persistent state.
--
-- The value must be serializable to JSON.
--
-- @
-- State.set "portfolio" (object ["AAPL" .= (100 :: Int)])
-- State.set "trade_count" (42 :: Int)
-- @
set :: ToJSON a => String -> a -> IO ()
set key value = do
    validateKey key
    stateDir <- getStateDir
    createDirectoryIfMissing True stateDir
    filepath <- getKeyPath key
    BL.writeFile filepath (encode value)

-- | Delete a key from persistent state.
--
-- Returns 'True' if the key existed and was deleted, 'False' otherwise.
--
-- @
-- deleted <- State.delete "old_data"
-- when deleted $ putStrLn "Cleaned up old data"
-- @
delete :: String -> IO Bool
delete key = do
    validateKey key
    filepath <- getKeyPath key
    exists' <- doesFileExist filepath
    if exists'
        then do
            removeFile filepath `catch` handleError
            return True
        else return False
  where
    handleError :: SomeException -> IO ()
    handleError _ = return ()

-- | List all keys in persistent state.
--
-- @
-- keys <- State.list
-- putStrLn $ "State contains " ++ show (length keys) ++ " keys"
-- @
list :: IO [String]
list = do
    stateDir <- getStateDir
    files <- listDirectory stateDir `catch` handleError
    return [dropExtension f | f <- files, takeExtension f == ".json"]
  where
    handleError :: SomeException -> IO [FilePath]
    handleError _ = return []

-- | Clear all state.
--
-- Removes all stored state keys.
--
-- @
-- State.clear
-- putStrLn "All state cleared"
-- @
clear :: IO ()
clear = do
    stateDir <- getStateDir
    files <- listDirectory stateDir `catch` handleListError
    mapM_ removeJsonFile files
  where
    handleListError :: SomeException -> IO [FilePath]
    handleListError _ = return []

    removeJsonFile :: FilePath -> IO ()
    removeJsonFile f = do
        stateDir <- getStateDir
        let filepath = stateDir </> f
        if takeExtension f == ".json"
            then removeFile filepath `catch` handleRemoveError
            else return ()

    handleRemoveError :: SomeException -> IO ()
    handleRemoveError _ = return ()

-- | Check if a key exists in state.
--
-- @
-- hasPortfolio <- State.exists "portfolio"
-- when hasPortfolio $ do
--     portfolio <- State.get "portfolio"
--     -- ...
-- @
exists :: String -> IO Bool
exists key = do
    validateKey key
    filepath <- getKeyPath key
    doesFileExist filepath

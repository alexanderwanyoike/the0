{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : The0.Input
-- Description : Input parsing and output formatting for the0 trading bots
-- License     : Apache-2.0
--
-- This module provides utilities for building trading bots on the0 platform.
--
-- = Example
--
-- @
-- import The0.Input
--
-- main :: IO ()
-- main = do
--     (botId, config) <- parse
--     putStrLn $ "Bot " ++ botId ++ " starting"
--     -- Your trading logic here
--     success "Bot executed successfully"
-- @
module The0.Input
    ( parse
    , parseAsMap
    , success
    , The0.Input.error
    , result
    , metric
    , The0.Input.log
    ) where

import Data.Aeson (Value(..), decode, encode, object, (.=), Object, toJSON)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (insert)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, SomeException)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- | Parse bot configuration from environment variables.
--
-- Returns a tuple of (botId, config) where config is an Aeson 'Value'.
--
-- Throws an error if BOT_ID or BOT_CONFIG environment variables are not set,
-- or if BOT_CONFIG is not valid JSON.
parse :: IO (String, Value)
parse = do
    botId <- lookupEnv "BOT_ID" >>= \case
        Just bid -> pure bid
        Nothing -> Prelude.error "BOT_ID environment variable not set"
    configStr <- lookupEnv "BOT_CONFIG" >>= \case
        Just cfg -> pure cfg
        Nothing -> Prelude.error "BOT_CONFIG environment variable not set"
    let config = fromMaybe (Prelude.error "Failed to parse BOT_CONFIG as JSON")
                           (decode (BL.pack configStr))
    pure (botId, config)

-- | Parse bot configuration with config as a Map.
--
-- Returns a tuple of (botId, config) where config is a 'Map.Map' String 'Value'.
--
-- Throws an error if BOT_ID or BOT_CONFIG environment variables are not set,
-- or if BOT_CONFIG is not valid JSON.
parseAsMap :: IO (String, Map.Map String Value)
parseAsMap = do
    botId <- lookupEnv "BOT_ID" >>= \case
        Just bid -> pure bid
        Nothing -> Prelude.error "BOT_ID environment variable not set"
    configStr <- lookupEnv "BOT_CONFIG" >>= \case
        Just cfg -> pure cfg
        Nothing -> Prelude.error "BOT_CONFIG environment variable not set"
    let config = fromMaybe (Prelude.error "Failed to parse BOT_CONFIG as JSON")
                           (decode (BL.pack configStr))
    pure (botId, config)

-- | Get the path to the result file.
resultFilePath :: IO FilePath
resultFilePath = do
    mountDir <- fromMaybe "bot" <$> lookupEnv "CODE_MOUNT_DIR"
    pure $ "/" ++ mountDir ++ "/result.json"

-- | Write result to the result file.
writeResult :: String -> IO ()
writeResult content = do
    path <- resultFilePath
    writeFile path content `catch` handleError
  where
    handleError :: SomeException -> IO ()
    handleError e = hPutStrLn stderr $ "RESULT_ERROR: Failed to write result file: " ++ show e

-- | Output a success result to the result file.
--
-- Writes a JSON object with status "success" and the provided message.
success :: String -> IO ()
success msg = do
    let escaped = escapeJson msg
    writeResult $ "{\"status\":\"success\",\"message\":\"" ++ escaped ++ "\"}"

-- | Output an error result to the result file and exit with code 1.
--
-- Writes a JSON object with status "error" and the provided message,
-- then terminates the process with exit code 1.
error :: String -> IO a
error msg = do
    let escaped = escapeJson msg
    writeResult $ "{\"status\":\"error\",\"message\":\"" ++ escaped ++ "\"}"
    exitWith (ExitFailure 1)

-- | Output a custom JSON result to the result file.
--
-- Serializes the provided 'Value' as JSON and writes it.
result :: Value -> IO ()
result val = writeResult $ BL.unpack (encode val)

-- | Escape special characters for JSON string output.
escapeJson :: String -> String
escapeJson = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '"'  = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar c    = [c]

-- | Emit a metric to stdout with timestamp.
--
-- The metric type and data are combined into a JSON object with
-- @_metric@ and @timestamp@ fields added.
metric :: String -> Value -> IO ()
metric metricType val = do
    ts <- getTimestamp
    let baseObj = case val of
            Object o -> o
            _ -> mempty
        withMeta = insert (fromString "_metric") (String $ T.pack metricType) $
                   insert (fromString "timestamp") (String $ T.pack ts) baseObj
    BL.putStrLn $ encode (Object withMeta)

-- | Log a message to stdout.
log :: String -> IO ()
log msg = do
    let escaped = escapeJson msg
    putStrLn $ "{\"message\":\"" ++ escaped ++ "\"}"

-- | Get current timestamp as milliseconds string with Z suffix.
getTimestamp :: IO String
getTimestamp = do
    t <- getPOSIXTime
    let millis = floor (t * 1000) :: Integer
    return $ show millis ++ "Z"

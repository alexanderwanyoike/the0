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
    , logInfo
    , logWarn
    , logError
    , LogLevel(..)
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
                   insert (fromString "timestamp") (toJSON ts) baseObj
    BL.putStrLn $ encode (Object withMeta)

-- | Log levels supported by the platform.
data LogLevel = Info | Warn | Error
    deriving (Show, Eq)

logLevelString :: LogLevel -> String
logLevelString Info = "info"
logLevelString Warn = "warn"
logLevelString Error = "error"

-- | Log a structured message to stderr.
--
-- Outputs a JSON object with level, message, timestamp, and any additional fields.
--
-- = Examples
--
-- @
-- -- Simple log (defaults to info level)
-- log "Starting trade execution" Nothing Nothing
--
-- -- Log with level
-- log "Connection lost" Nothing (Just Warn)
--
-- -- Log with structured data
-- log "Order placed" (Just $ object ["order_id" .= "12345", "symbol" .= "BTC/USD"]) Nothing
--
-- -- Log with data and level
-- log "Order failed" (Just $ object ["order_id" .= "12345"]) (Just Error)
-- @
log :: String -> Maybe Value -> Maybe LogLevel -> IO ()
log msg mData mLevel = do
    ts <- getTimestamp
    let level = maybe Info id mLevel
        baseObj = case mData of
            Just (Object o) -> o
            _ -> mempty
        withFields = insert (fromString "level") (String $ T.pack $ logLevelString level) $
                     insert (fromString "message") (String $ T.pack msg) $
                     insert (fromString "timestamp") (toJSON ts) baseObj
    BL.hPutStrLn stderr $ encode (Object withFields)

-- | Convenience function: log an info message
logInfo :: String -> Maybe Value -> IO ()
logInfo msg mData = The0.Input.log msg mData (Just Info)

-- | Convenience function: log a warning message
logWarn :: String -> Maybe Value -> IO ()
logWarn msg mData = The0.Input.log msg mData (Just Warn)

-- | Convenience function: log an error message
logError :: String -> Maybe Value -> IO ()
logError msg mData = The0.Input.log msg mData (Just Error)

-- | Get current timestamp as milliseconds since Unix epoch.
getTimestamp :: IO Integer
getTimestamp = do
    t <- getPOSIXTime
    return $ floor (t * 1000)

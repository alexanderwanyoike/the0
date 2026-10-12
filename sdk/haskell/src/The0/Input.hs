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
    , successWithData
    , The0.Input.error
    , errorWithData
    , result
    , metric
    , The0.Input.log
    , logInfo
    , logWarn
    , logError
    , LogLevel(..)
    ) where

import Data.Aeson (Value(..), decodeStrict, encode, object, (.=), ToJSON, toJSON)
import Data.Aeson.Types (Pair)
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap (insert)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import The0.Internal.Env (lookupEnvBytes)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Control.Exception (catch, evaluate, SomeException)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

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
    configBytes <- lookupEnvBytes "BOT_CONFIG" >>= \case
        Just cfg -> pure cfg
        Nothing -> Prelude.error "BOT_CONFIG environment variable not set"
    let config = fromMaybe (Prelude.error "Failed to parse BOT_CONFIG as JSON")
                           (decodeStrict configBytes)
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
    configBytes <- lookupEnvBytes "BOT_CONFIG" >>= \case
        Just cfg -> pure cfg
        Nothing -> Prelude.error "BOT_CONFIG environment variable not set"
    let config = fromMaybe (Prelude.error "Failed to parse BOT_CONFIG as JSON")
                           (decodeStrict configBytes)
    pure (botId, config)

-- | Get the path to the result file.
resultFilePath :: IO FilePath
resultFilePath = do
    mountDir <- fromMaybe "bot" <$> lookupEnv "CODE_MOUNT_DIR"
    pure $ "/" ++ mountDir ++ "/result.json"

-- | Write result to the result file.
writeResult :: BL.ByteString -> IO ()
writeResult content = do
    -- Encoded before the file is opened, so a value that fails to evaluate
    -- fails the bot instead of leaving an empty result behind.
    _ <- evaluate (BL.length content)
    path <- resultFilePath
    BL.writeFile path content `catch` handleError
  where
    handleError :: SomeException -> IO ()
    handleError e = hPutStrLn stderr $ "RESULT_ERROR: Failed to write result file: " ++ show e

-- | Output a success result to the result file.
--
-- Writes a JSON object with status "success" and the provided message.
success :: String -> IO ()
success msg = result $ statusResult "success" msg []

-- | Output an error result to the result file and exit with code 1.
--
-- Writes a JSON object with status "error" and the provided message,
-- prints the message on stderr as an @error@ log line, then terminates the
-- process with exit code 1.
error :: String -> IO a
error msg = failWith msg []

-- | Output a success result carrying @data@ to the result file.
successWithData :: ToJSON a => String -> a -> IO ()
successWithData msg payload = result $ statusResult "success" msg ["data" .= payload]

-- | Output an error result carrying @data@ to the result file and exit with
-- code 1, as 'The0.Input.error' does.
errorWithData :: ToJSON a => String -> a -> IO b
errorWithData msg payload = failWith msg ["data" .= payload]

statusResult :: String -> String -> [Pair] -> Value
statusResult status msg extra = object $ ["status" .= status, "message" .= msg] ++ extra

failWith :: String -> [Pair] -> IO a
failWith msg extra = do
    result $ statusResult "error" msg extra
    logError msg Nothing
    exitWith (ExitFailure 1)

-- | Output a custom JSON result to the result file.
--
-- Serializes the provided 'Value' as JSON and writes it.
result :: Value -> IO ()
result val = writeResult (encode val)

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
    -- stdout is a pipe in a container, which GHC block-buffers, so a
    -- realtime bot's metrics would otherwise reach its logs late.
    hFlush stdout

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

-- | The current time as a UTC ISO 8601 string with milliseconds, such as
-- @2026-10-12T09:30:00.123Z@.
getTimestamp :: IO String
getTimestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" <$> getCurrentTime

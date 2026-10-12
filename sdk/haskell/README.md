# the0-sdk (Haskell)

This SDK provides utilities for building trading bots on the0 platform using Haskell.

## Installation

### Option 1: Git Dependency (Recommended)

Add to your `cabal.project`:

```
packages: .

source-repository-package
    type: git
    location: https://github.com/alexanderwanyoike/the0.git
    tag: v1.18.0
    subdir: sdk/haskell
```

Then add to your `.cabal` file:

```cabal
build-depends: the0-sdk
```

> **Note:** `v1.18.0` is the first release with SDK 0.4, which follows the [bot contract](https://github.com/alexanderwanyoike/the0/blob/dev/docs/custom-bot-development/bot-contract.md). Check [releases](https://github.com/alexanderwanyoike/the0/releases) for later tags.

### Option 2: Copy the Package

Copy the `src/The0/` directory to your project's source directory, and add the packages listed under `build-depends` in `the0-sdk.cabal` to your own.

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Data.Aeson (Value(..))

main :: IO ()
main = do
    (botId, config) <- parse
    putStrLn $ "Bot " ++ botId ++ " starting"

    -- Access config values
    -- config is an Aeson Value (JSON)

    -- Your trading logic here

    success "Bot executed successfully"
```

## API Reference

### `parse :: IO (String, Value)`

Parse bot configuration from environment variables.

Returns a tuple of `(botId, config)` where:
- `botId` is the unique bot instance identifier
- `config` is the bot configuration as an Aeson `Value`

**Throws:** Error if `BOT_ID` or `BOT_CONFIG` environment variables are not set.

### `parseAsMap :: IO (String, Map String Value)`

Parse bot configuration with config as a Map for easier field access.

Returns a tuple of `(botId, config)` where:
- `botId` is the unique bot instance identifier
- `config` is the bot configuration as a `Map String Value`

### `success :: String -> IO ()`

Write a success result to the result file, `/bot/result.json`:
`{"status":"success","message":"<your message>"}`

### `successWithData :: ToJSON a => String -> a -> IO ()`

Write a success result that carries data:
`{"status":"success","message":"<your message>","data":<data>}`

```haskell
successWithData "Bought 0.5 BTC" (object ["order_id" .= ("o-1" :: String), "filled" .= (0.5 :: Double)])
```

### `error :: String -> IO a`

Report a failure, in three steps:
1. Writes an error result: `{"status":"error","message":"<your message>"}`
2. Prints the message on stderr as an `error` log line, so the failure shows in the bot's logs.
3. Exits with code 1. Nothing after it runs.

### `errorWithData :: ToJSON a => String -> a -> IO b`

Like `error`, with a result that carries data:
`{"status":"error","message":"<your message>","data":<data>}`. The log line carries only the message.

### `result :: Value -> IO ()`

Write a custom JSON result to the result file. Each result call replaces the file, so the last call wins.

### `metric :: String -> Value -> IO ()`

Emit a metric for the platform to collect.

```haskell
metric "price" (object ["symbol" .= ("BTC/USD" :: String), "value" .= (45000 :: Double)])
```

Prints one JSON line to stdout with `_metric` and a UTC ISO 8601 `timestamp`, such as `"2026-10-12T09:30:00.123Z"`, added to the fields of the data.

### `log :: String -> Maybe Value -> Maybe LogLevel -> IO ()`

Log a structured message to stderr, as one JSON line with `level`, `message` and a UTC ISO 8601 `timestamp`.

```haskell
-- Simple log (defaults to info level)
log "Starting bot" Nothing Nothing

-- Log with level
log "Connection lost" Nothing (Just Warn)

-- Log with structured data
log "Order placed" (Just $ object ["order_id" .= "123"]) Nothing

-- Log with data and level
log "Order failed" (Just $ object ["order_id" .= "123"]) (Just Error)
```

### `logInfo`, `logWarn`, `logError :: String -> Maybe Value -> IO ()`

Convenience functions for logging at specific levels.

```haskell
logInfo "Bot started" Nothing
logWarn "Rate limit approaching" (Just $ object ["remaining" .= (10 :: Int)])
logError "Connection failed" (Just $ object ["error" .= "timeout"])
```

## State (`The0.State`)

State persists between runs, one JSON file per key.

- `get :: FromJSON a => String -> IO (Maybe a)` returns `Nothing` when the key does not exist.
- `set :: ToJSON a => String -> a -> IO ()` stores any JSON value.
- `delete :: String -> IO Bool` returns whether the key existed, and throws when its file cannot be removed.
- `list :: IO [String]`, `exists :: String -> IO Bool` and `clear :: IO ()`.

A key must not be empty or contain `/`, `\` or `..`. During a query, `set`, `delete` and `clear` throw `ReadOnlyStateError` before they check the key.

## Queries (`The0.Query`)

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified The0.Query as Query
import qualified The0.State as State
import Data.Aeson (Value, object, (.=))
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    Query.handler "/signals" $ \req -> do
        let limit = fromMaybe 10 (Query.getParamAs req "limit") :: Int
            symbol = Query.getParam req "symbol" "BTC/USD"
        signals <- fromMaybe [] <$> State.get "signals" :: IO [Value]
        return $ object ["symbol" .= symbol, "signals" .= take limit signals]

    Query.handler "/trade" $ \req ->
        case Query.getParamAs req "id" :: Maybe Int of
            Nothing -> fail "id is required"
            Just tradeId -> return $ object ["id" .= tradeId]

    Query.run
```

`Query.run` picks its mode from the environment: one ephemeral query when `QUERY_PATH` is set, the query server on port 9476 when `BOT_TYPE` is `realtime`, and `/info` otherwise. `/health` and `/info` are built in.

### Params

Params are JSON values. An ephemeral query reads them from `QUERY_PARAMS`. The query server reads them from a JSON object body, or else from the URL query string, where every value is a string. Params that are not a JSON object, or that repeat a key, are unreadable: an ephemeral query gets no params, and the server uses the query string.

| Accessor | Gives |
|---|---|
| `getParam req key default` | the param as text: a string as it is, any other value as its JSON text, and a `null` as missing |
| `reqParams req` | every param as text, the same way, without the `null`s |
| `getParamAs req key :: Maybe a` | the param decoded into `a`. A string that parses as `a` counts, so `"5"` from a query string reads as `5 :: Int`. Asked for text, it gives a non-string as its JSON text and a `null` as `Nothing`. |
| `reqValues req` | every param with its JSON type |

To unit test a handler, build its request with `Request path textParams` or `RequestValues path jsonParams`.

### Errors

A handler reports an error by throwing any exception: with `fail`, with `throwIO`, or from pure code that calls `error`. The query answers `{"status":"error","error":"<message>"}`. The query server responds 500 and keeps serving, and an ephemeral query exits 1. A path with no handler answers 404, with the paths the bot answers under `available`.

`ReadOnlyStateError` is the one error type state throws during a query. `The0.Query` and `The0.State` both export it.

## Example Bot

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson
import qualified Data.Map.Strict as Map
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    (botId, config) <- parseAsMap
    hPutStrLn stderr $ "Bot " ++ botId ++ " starting"

    -- Access configuration
    let symbol = Map.lookup "symbol" config

    case symbol of
        Just (String s) -> do
            hPutStrLn stderr $ "Trading symbol: " ++ show s
            success "Trade executed"
        _ -> The0.Input.error "Missing symbol configuration"
```

## Building Your Bot

```bash
# Initialize a new Haskell project
cabal init --minimal

# Add the0 SDK as a dependency in your .cabal file

# Build your bot
cabal build --enable-optimization=2

# The binary will be in dist-newstyle/build/...
```

## License

Apache-2.0

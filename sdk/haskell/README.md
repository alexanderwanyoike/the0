# the0 SDK for Haskell

This SDK provides utilities for building trading bots on the0 platform using Haskell.

## Installation

Add `the0` to your `.cabal` file dependencies:

```cabal
build-depends: the0
```

Or if using as a local package, add to your `cabal.project`:

```
packages: .
          path/to/the0
```

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

Output a success result to stdout.

Prints a JSON object: `{"status":"success","message":"<your message>"}`

### `error :: String -> IO a`

Output an error result to stdout and exit with code 1.

Prints a JSON object: `{"status":"error","message":"<your message>"}`

### `result :: Value -> IO ()`

Output a custom JSON result to stdout.

Serializes the provided `Value` as JSON and prints it.

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

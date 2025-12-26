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
    tag: v1.1.0
    subdir: sdk/haskell
```

Then add to your `.cabal` file:

```cabal
build-depends: the0-sdk
```

> **Note:** Replace `v1.1.0` with the latest release tag.

### Option 2: Copy the Package

Copy `src/The0/Input.hs` to your project's source directory.

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

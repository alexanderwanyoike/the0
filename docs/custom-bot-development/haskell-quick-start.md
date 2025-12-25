---
title: "Haskell Quick Start"
description: "Build your first Haskell trading bot with the0"
tags: ["custom-bots", "haskell", "quick-start"]
order: 13
---

# Haskell Quick Start Guide

Build a functional, type-safe trading bot in Haskell with the0's SDK.

---

## Prerequisites

- GHC 9.6+ and Cabal installed
- the0 CLI installed
- Valid the0 API key

---

## Project Structure

```
my-haskell-bot/
├── my-haskell-bot.cabal   # Package definition
├── cabal.project          # Cabal project config (optional)
├── app/
│   └── Main.hs            # Your bot entry point
├── bot-config.yaml        # Bot configuration
├── bot-schema.json        # Parameter schema
└── README.md              # Documentation
```

---

## Step 1: Create Your Project

```bash
# Create project directory
mkdir my-haskell-bot
cd my-haskell-bot

# Initialize Cabal project
cabal init --minimal --exe --main-is=app/Main.hs
```

---

## Step 2: Configure Your .cabal File

Edit your `.cabal` file to add the the0 SDK:

```cabal
cabal-version:      3.0
name:               my-haskell-bot
version:            0.1.0.0
build-type:         Simple

executable my-haskell-bot
    main-is:          Main.hs
    hs-source-dirs:   app
    build-depends:    base ^>=4.18,
                      the0,
                      aeson ^>=2.2
    default-language: Haskell2010
    ghc-options:      -Wall -O2
```

Add the SDK via `cabal.project`:

```
packages: .

source-repository-package
    type: git
    location: https://github.com/alexanderwanyoike/the0
    subdir: sdk/haskell
```

---

## Step 3: Write Your Bot

Create `app/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    -- Parse bot configuration
    (botId, config) <- parse

    hPutStrLn stderr $ "Bot " ++ botId ++ " starting..."

    -- Access configuration values
    case config of
        Object obj -> do
            case KM.lookup "symbol" obj of
                Just (String symbol) ->
                    hPutStrLn stderr $ "Trading symbol: " ++ show symbol
                _ -> pure ()

            case KM.lookup "amount" obj of
                Just (Number amount) ->
                    hPutStrLn stderr $ "Trade amount: " ++ show amount
                _ -> pure ()
        _ -> pure ()

    -- Your trading logic here
    -- Example: Check price, execute trade, log results

    -- Signal success when done
    success "Bot executed successfully"
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-haskell-bot
description: "A functional Haskell trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: ghc96

entrypoints:
  bot: app/Main.hs

schema:
  bot: bot-schema.json

readme: README.md
```

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the Haskell trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair symbol",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount to trade per execution",
      "default": 100
    }
  },
  "required": ["symbol"]
}
```

---

## Step 6: Deploy

```bash
# Deploy your bot
the0 custom-bot deploy
```

The build happens automatically in the cloud - no need to compile locally!

---

## SDK API Reference

The `The0.Input` module provides these functions:

### `parse :: IO (String, Value)`

Parse bot configuration from environment. Returns `(botId, config)`.

```haskell
(botId, config) <- parse
```

### `parseAsMap :: IO (String, Map String Value)`

Parse configuration as a Map for easier access.

```haskell
(botId, config) <- parseAsMap
case Map.lookup "symbol" config of
    Just (String s) -> putStrLn $ "Symbol: " ++ show s
    _ -> pure ()
```

### `success :: String -> IO ()`

Output a success result.

```haskell
success "Trade executed successfully"
```

### `error :: String -> IO a`

Output an error result and exit with code 1.

```haskell
The0.Input.error "Failed to connect to exchange"
```

### `result :: Value -> IO ()`

Output a custom JSON result.

```haskell
import Data.Aeson (object, (.=))

result $ object
    [ "status" .= ("success" :: String)
    , "trade_id" .= ("12345" :: String)
    , "filled_amount" .= (0.5 :: Double)
    ]
```

---

## Adding Dependencies

Add any package from Hackage to your `.cabal` file:

```cabal
build-depends:    base ^>=4.18,
                  the0,
                  aeson ^>=2.2,
                  http-conduit ^>=2.3,
                  time ^>=1.12,
                  text ^>=2.0
```

The CLI builds your bot before deployment - no need to compile locally!

---

## Example: HTTP Request Bot

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BL
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    (botId, config) <- parse

    -- Get API endpoint from config
    let endpoint = case config of
            Object obj -> case KM.lookup "api_endpoint" obj of
                Just (String url) -> show url
                _ -> "https://api.example.com/price"
            _ -> "https://api.example.com/price"

    hPutStrLn stderr $ "Bot " ++ botId ++ " fetching from " ++ endpoint

    -- Make HTTP request
    response <- httpLBS =<< parseRequest endpoint

    case decode (getResponseBody response) :: Maybe Value of
        Just jsonData -> do
            hPutStrLn stderr $ "Received: " ++ show jsonData
            success "Data fetched successfully"
        Nothing -> The0.Input.error "Failed to parse response"
```

---

## Best Practices

### Error Handling

Use Haskell's type system for robust error handling:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Control.Exception (try, SomeException)

executeTrade :: String -> Double -> IO (Either String String)
executeTrade symbol amount = do
    -- Your trade logic
    pure $ Right "trade_id_123"

main :: IO ()
main = do
    (botId, config) <- parse

    result <- executeTrade "BTC/USDT" 100.0
    case result of
        Right tradeId -> do
            putStrLn $ "Trade executed: " ++ tradeId
            success $ "Trade " ++ tradeId ++ " completed"
        Left err ->
            The0.Input.error $ "Trade failed: " ++ err
```

### Configuration Validation

Validate configuration early with pattern matching:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    (botId, config) <- parse

    -- Validate required fields
    case config of
        Object obj -> case KM.lookup "symbol" obj of
            Just (String symbol) -> do
                let amount = case KM.lookup "amount" obj of
                        Just (Number n) -> realToFrac n
                        _ -> 100.0 :: Double

                hPutStrLn stderr $ "Trading " ++ show symbol ++ " with amount " ++ show amount
                success "Validation passed"

            _ -> The0.Input.error "Missing required field: symbol"

        _ -> The0.Input.error "Invalid configuration format"
```

### Logging

Use stderr for logs (stdout is reserved for results):

```haskell
import System.IO (hPutStrLn, stderr)

hPutStrLn stderr "DEBUG: Processing trade..."  -- Logs to stderr
putStrLn "..."  -- Reserved for JSON result output
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Deployment Guide](/custom-bot-development/deployment)

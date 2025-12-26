---
title: "Haskell Quick Start"
description: "Build your first Haskell trading bot with the0"
tags: ["custom-bots", "haskell", "ghc", "quick-start"]
order: 15
---

# Haskell Quick Start Guide

Build pure, type-safe trading bots in Haskell with the0's GHC 9.6 runtime. Haskell's strong type system and functional paradigm make it excellent for expressing complex trading logic with mathematical precision.

---

## Why Haskell for Trading Bots?

Haskell offers unique advantages for algorithmic trading:

- **Pure Functions**: Referential transparency makes strategies easier to reason about
- **Strong Type System**: Catch configuration and logic errors at compile time
- **Algebraic Data Types**: Express market conditions and signals precisely
- **Pattern Matching**: Clean handling of different market scenarios
- **Immutability by Default**: No accidental state mutations during trades
- **Lazy Evaluation**: Efficient processing of large data streams

**When to Choose Haskell:**
- Complex strategies with many conditional branches
- When correctness is paramount (the type system catches bugs)
- Mathematical or quantitative strategies
- Teams with functional programming experience

**Popular Hackage Packages for Trading:**
- `http-conduit` - HTTP client for REST APIs
- `aeson` - Fast JSON parsing and encoding
- `scientific` - Arbitrary precision arithmetic
- `time` - Date and time handling
- `async` - Concurrent operations
- `lens` - Composable data access

---

## Prerequisites

- GHC 9.6+ and Cabal installed ([ghcup](https://www.haskell.org/ghcup/) recommended)
- the0 CLI installed
- Valid the0 API key
- Basic understanding of Haskell syntax and types

---

## Project Structure

```
my-haskell-bot/
├── my-haskell-bot.cabal   # Package definition with dependencies
├── cabal.project          # Cabal project config
├── app/
│   └── Main.hs            # Your bot entry point
├── bot-config.yaml        # Bot configuration
├── bot-schema.json        # Parameter schema
├── config.json            # Example configuration
└── README.md              # Documentation
```

---

## Step 1: Create Your Project

```bash
mkdir my-haskell-bot
cd my-haskell-bot
mkdir app
```

---

## Step 2: Create Your .cabal File

Create `my-haskell-bot.cabal`:

```cabal
cabal-version:      3.0
name:               my-haskell-bot
version:            0.1.0.0
build-type:         Simple

executable my-haskell-bot
    main-is:          Main.hs
    hs-source-dirs:   app
    build-depends:    base ^>=4.18,
                      the0-sdk,
                      aeson ^>=2.2,
                      text ^>=2.0
    default-language: Haskell2010
    ghc-options:      -Wall -O2
```

Create `cabal.project` to add the SDK:

```
packages: .

source-repository-package
    type: git
    location: https://github.com/alexanderwanyoike/the0.git
    tag: v1.1.0
    subdir: sdk/haskell
```

> **Note:** Replace `v1.1.0` with the latest release tag. Alternatively, copy `src/The0/Input.hs` from the SDK to your project.

---

## Step 3: Write Your Bot

Create `app/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import System.IO (hPutStrLn, stderr)

{-|
Main entry point for the trading bot.

The the0 SDK handles:
- Reading BOT_ID and BOT_CONFIG from environment
- JSON parsing with aeson
- Writing results to the correct output file
-}
main :: IO ()
main = do
    -- Parse bot configuration from environment
    -- Returns (botId, config) where config is an aeson Value
    (botId, config) <- parse

    hPutStrLn stderr $ "Bot " ++ botId ++ " starting..."

    -- Extract configuration with pattern matching
    let (symbol, amount) = extractConfig config

    hPutStrLn stderr $ "Trading " ++ symbol ++ " with amount " ++ show amount

    -- ===========================================
    -- YOUR TRADING LOGIC GOES HERE
    -- ===========================================

    -- Example: Validate configuration
    if amount <= 0
        then The0.Input.error "Amount must be positive"
        else do
            -- Example: Fetch market data
            -- price <- fetchPrice symbol

            -- Example: Execute trade
            -- tradeResult <- executeTrade symbol amount

            -- ===========================================
            -- END OF TRADING LOGIC
            -- ===========================================

            -- Output result with additional data
            result $ object
                [ "status" .= ("success" :: String)
                , "message" .= ("Trade executed" :: String)
                , "data" .= object
                    [ "symbol" .= symbol
                    , "amount" .= amount
                    ]
                ]

-- | Extract configuration values with defaults
extractConfig :: Value -> (String, Double)
extractConfig (Object obj) =
    let symbol = case KM.lookup "symbol" obj of
            Just (String s) -> show s
            _ -> "BTC/USDT"
        amount = case KM.lookup "amount" obj of
            Just (Number n) -> realToFrac n
            _ -> 100.0
    in (symbol, amount)
extractConfig _ = ("BTC/USDT", 100.0)
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-haskell-bot
description: "A pure functional Haskell trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: ghc96

# The entrypoint is the source file - gets compiled automatically
entrypoints:
  bot: app/Main.hs

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto]
  tags: [haskell, functional, type-safe]
```

**Note:** The `runtime: ghc96` tells the platform to compile your bot with GHC 9.6. You don't need to compile locally.

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Haskell Bot Configuration",
  "description": "Configuration for the Haskell trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair (e.g., BTC/USDT)",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount in base currency to trade",
      "default": 100,
      "minimum": 0.01
    },
    "api_key": {
      "type": "string",
      "title": "API Key",
      "description": "Your exchange API key"
    },
    "api_secret": {
      "type": "string",
      "title": "API Secret",
      "description": "Your exchange API secret"
    }
  },
  "required": ["symbol", "api_key", "api_secret"]
}
```

---

## Step 6: Test Locally

```bash
# Build the project
cabal build

# Set environment variables for testing
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USDT","amount":100}'
export CODE_MOUNT_DIR="/tmp"

# Run
cabal run
```

---

## Step 7: Deploy

```bash
the0 custom-bot deploy
```

The platform will:
1. Compile your Haskell code with GHC 9.6
2. Link required libraries
3. Deploy the binary to the runtime

No need to compile locally - it all happens in the cloud!

---

## SDK API Reference

The `The0.Input` module provides these functions:

### `parse :: IO (String, Value)`

Parse bot configuration from environment variables:

```haskell
(botId, config) <- parse

-- config is an aeson Value
-- Access with pattern matching:
case config of
    Object obj -> case KM.lookup "symbol" obj of
        Just (String s) -> putStrLn $ "Symbol: " ++ show s
        _ -> pure ()
    _ -> pure ()
```

### `parseAsMap :: IO (String, Map String Value)`

Parse configuration as a Map for easier key lookup:

```haskell
import qualified Data.Map as Map

(botId, config) <- parseAsMap

case Map.lookup "symbol" config of
    Just (String s) -> putStrLn $ "Symbol: " ++ show s
    _ -> pure ()
```

### `success :: String -> IO ()`

Output a success result:

```haskell
success "Trade executed successfully"
-- Outputs: {"status":"success","message":"Trade executed successfully"}
```

### `error :: String -> IO a`

Output an error result and exit with code 1:

```haskell
if amount <= 0
    then The0.Input.error "Amount must be positive"
    -- Program exits here
    else continueWithTrade
-- This line never reached if amount <= 0
```

### `result :: Value -> IO ()`

Output a custom JSON result:

```haskell
import Data.Aeson (object, (.=))

result $ object
    [ "status" .= ("success" :: String)
    , "trade_id" .= ("abc123" :: String)
    , "filled_amount" .= (0.5 :: Double)
    , "average_price" .= (45123.50 :: Double)
    ]
```

---

## Example: Price Fetcher with http-conduit

Here's a complete example that fetches real price data:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson (Value(..), decode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    (botId, config) <- parse

    let symbol = case config of
            Object obj -> case KM.lookup "symbol" obj of
                Just (String s) -> filter (/= '/') $ show s  -- Remove quotes and /
                _ -> "BTCUSDT"
            _ -> "BTCUSDT"

    hPutStrLn stderr $ "Bot " ++ botId ++ " fetching price for " ++ symbol

    priceResult <- fetchBinancePrice symbol
    case priceResult of
        Right price -> do
            hPutStrLn stderr $ "Current price: $" ++ show price
            result $ object
                [ "status" .= ("success" :: String)
                , "message" .= ("Price fetched" :: String)
                , "data" .= object
                    [ "symbol" .= symbol
                    , "price" .= price
                    ]
                ]
        Left err ->
            The0.Input.error $ "Failed to fetch price: " ++ err

-- | Fetch price from Binance API
fetchBinancePrice :: String -> IO (Either String Double)
fetchBinancePrice symbol = do
    let url = "https://api.binance.com/api/v3/ticker/price?symbol=" ++ symbol
    result <- try $ do
        request <- parseRequest url
        response <- httpLBS request
        return $ getResponseBody response
    case result of
        Left (e :: SomeException) -> return $ Left $ show e
        Right body -> case decode body :: Maybe Value of
            Just (Object obj) -> case KM.lookup "price" obj of
                Just (String priceStr) ->
                    case reads (show priceStr) :: [(Double, String)] of
                        [(p, _)] -> return $ Right p
                        _ -> return $ Left "Failed to parse price"
                _ -> return $ Left "Missing price field"
            _ -> return $ Left "Invalid response format"
```

Add to your `.cabal` file:

```cabal
build-depends:    base ^>=4.18,
                  the0,
                  aeson ^>=2.2,
                  text ^>=2.0,
                  http-conduit ^>=2.3,
                  bytestring ^>=0.11
```

---

## Best Practices

### 1. Use Algebraic Data Types for Signals

Express trading signals with precise types:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Data.Aeson (ToJSON(..), object, (.=))

data TradeSignal
    = Buy { buySymbol :: String, buyAmount :: Double, buyPrice :: Double }
    | Sell { sellSymbol :: String, sellAmount :: Double, sellPrice :: Double }
    | Hold { holdReason :: String }

instance ToJSON TradeSignal where
    toJSON (Buy sym amt price) = object
        [ "action" .= ("buy" :: String)
        , "symbol" .= sym
        , "amount" .= amt
        , "price" .= price
        ]
    toJSON (Sell sym amt price) = object
        [ "action" .= ("sell" :: String)
        , "symbol" .= sym
        , "amount" .= amt
        , "price" .= price
        ]
    toJSON (Hold reason) = object
        [ "action" .= ("hold" :: String)
        , "reason" .= reason
        ]

executeSignal :: TradeSignal -> IO ()
executeSignal signal = result $ toJSON signal
```

### 2. Pattern Matching for Configuration

Use pattern matching for clean config extraction:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM

main :: IO ()
main = do
    (botId, config) <- parse

    case config of
        Object obj -> case (KM.lookup "symbol" obj, KM.lookup "api_key" obj) of
            (Just (String symbol), Just (String apiKey)) -> do
                -- Both required fields present
                runBot (show symbol) (show apiKey)

            (Nothing, _) ->
                The0.Input.error "Missing required field: symbol"

            (_, Nothing) ->
                The0.Input.error "Missing required field: api_key"

        _ ->
            The0.Input.error "Invalid configuration format"

runBot :: String -> String -> IO ()
runBot symbol apiKey = do
    -- Your trading logic here
    success $ "Trading " ++ symbol
```

### 3. Error Handling with Either

Use Either for composable error handling:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import The0.Input
import Control.Monad (when)

type TradingError = String

executeTrade :: String -> Double -> IO (Either TradingError String)
executeTrade symbol amount
    | amount <= 0 = return $ Left "Amount must be positive"
    | otherwise = do
        -- Your trade logic here
        return $ Right "trade_12345"

main :: IO ()
main = do
    (botId, config) <- parse

    tradeResult <- executeTrade "BTC/USDT" 100.0
    case tradeResult of
        Right tradeId -> success $ "Trade completed: " ++ tradeId
        Left err -> The0.Input.error err
```

### 4. Logging

Both stdout and stderr go to your bot's logs:

```haskell
import System.IO (hPutStrLn, stderr)

putStrLn "Starting trade..."              -- Goes to log
hPutStrLn stderr "DEBUG: Details..."      -- Goes to log
```

### 5. Precise Arithmetic with Scientific

Use the `scientific` package for exact decimal arithmetic:

```haskell
import Data.Scientific (Scientific, fromFloatDigits)

-- Avoid floating point errors
price :: Scientific
price = fromFloatDigits 45123.50

quantity :: Scientific
quantity = fromFloatDigits 0.5

total :: Scientific
total = price * quantity  -- Exact: 22561.75
```

---

## Adding Dependencies

Add packages from Hackage to your `.cabal` file:

```cabal
build-depends:    base ^>=4.18,
                  the0,
                  aeson ^>=2.2,
                  text ^>=2.0,
                  http-conduit ^>=2.3,
                  bytestring ^>=0.11,
                  time ^>=1.12,
                  scientific ^>=0.3,
                  containers ^>=0.6
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)

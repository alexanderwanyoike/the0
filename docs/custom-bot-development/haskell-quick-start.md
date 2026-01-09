---
title: "Haskell Quick Start"
description: "Build your first Haskell trading bot with the0"
tags: ["custom-bots", "haskell", "ghc", "quick-start"]
order: 16
---

# Haskell Quick Start

Haskell's strong type system and pure functional paradigm make it excellent for expressing complex trading logic with mathematical precision. The compiler catches entire classes of bugs at compile time, and referential transparency makes strategies easier to reason about. This guide walks through building an SMA crossover bot that calculates indicators and emits signals.

By the end of this guide, you'll have a working scheduled bot that calculates Simple Moving Averages and detects crossover signals using market data.

## Prerequisites

Before starting, ensure you have the CLI installed and authenticated:

```bash
# Clone the repository and build the CLI
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli
make install

# Authenticate
the0 auth login
```

You'll also need Docker installed - the CLI uses Docker for building Haskell projects with the correct GHC version. Optionally, install GHC 9.6+ and Cabal locally for development using [ghcup](https://www.haskell.org/ghcup/).

## Project Structure

Create a new directory for your bot:

```bash
mkdir sma-bot
cd sma-bot
mkdir app
```

A Haskell bot requires these files:

```
sma-bot/
├── sma-bot.cabal            # Package definition
├── cabal.project            # Cabal project config
├── app/
│   └── Main.hs              # Bot entry point
├── bot-config.yaml          # Bot metadata and runtime settings
├── bot-schema.json          # Configuration schema for users
└── bin/                     # Compiled binaries (created by the0 build)
    └── sma-bot              # Your executable
```

The CLI automatically builds your Haskell project and copies executables to a simple `bin/` directory.

## Defining Bot Metadata

Create `bot-config.yaml`:

```yaml
name: sma-crossover
description: "SMA crossover strategy bot with Yahoo Finance data"
version: 1.0.0
author: "your-name"
type: scheduled
runtime: ghc96

entrypoints:
  bot: bin/sma-bot

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover, haskell, functional]
  complexity: intermediate
```

The `entrypoints.bot` field points to the compiled binary in `bin/`. The CLI automatically copies built executables there during the build process.

## Defining Configuration Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "SMA Crossover Configuration",
  "description": "Configuration for the SMA crossover trading strategy bot",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Stock symbol to monitor (e.g., AAPL, MSFT, GOOGL)",
      "default": "AAPL"
    },
    "short_period": {
      "type": "integer",
      "description": "Number of periods for short SMA (fast moving average)",
      "default": 5,
      "minimum": 2,
      "maximum": 50
    },
    "long_period": {
      "type": "integer",
      "description": "Number of periods for long SMA (slow moving average)",
      "default": 20,
      "minimum": 5,
      "maximum": 200
    }
  },
  "additionalProperties": false
}
```

## Configuring Cabal

Create `sma-bot.cabal`:

```cabal
cabal-version:      3.0
name:               sma-bot
version:            1.0.0
build-type:         Simple

executable sma-bot
    main-is:          Main.hs
    hs-source-dirs:   app
    build-depends:    base ^>=4.18,
                      the0-sdk,
                      aeson ^>=2.2,
                      text ^>=2.0,
                      http-conduit ^>=2.3,
                      bytestring ^>=0.11,
                      scientific ^>=0.3
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

## Writing the Bot Logic

Create `app/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import The0.Input
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Simple
import Data.Scientific (toRealFloat)
import System.IO (hPutStrLn, stderr)
import Control.Exception (try, SomeException)

main :: IO ()
main = do
    -- Parse configuration from environment
    (botId, config) <- parse

    let (symbol, shortPeriod, longPeriod) = extractConfig config

    hPutStrLn stderr $ "Bot " ++ botId ++ " started - " ++ symbol ++
                       " SMA(" ++ show shortPeriod ++ "/" ++ show longPeriod ++ ")"

    -- Fetch and process data
    pricesResult <- fetchYahooFinance symbol
    case pricesResult of
        Left err -> The0.Input.error $ "Failed to fetch data: " ++ err
        Right prices ->
            if length prices < longPeriod
                then do
                    hPutStrLn stderr $ "Insufficient data: need " ++ show longPeriod ++
                                      " prices, have " ++ show (length prices)
                    success "Insufficient data for analysis"
                else do
                    let currentPrice = last prices
                        previousPrice = prices !! (length prices - 2)
                        changePct = (currentPrice - previousPrice) / previousPrice * 100

                    -- Emit price metric
                    metric "price" $ object
                        [ "symbol" .= symbol
                        , "value" .= roundTo currentPrice 2
                        , "change_pct" .= roundTo changePct 3
                        ]

                    -- Calculate SMAs
                    let shortSma = calculateSma prices shortPeriod
                        longSma = calculateSma prices longPeriod

                    -- Emit SMA metric
                    metric "sma" $ object
                        [ "symbol" .= symbol
                        , "short_sma" .= roundTo shortSma 2
                        , "long_sma" .= roundTo longSma 2
                        , "short_period" .= shortPeriod
                        , "long_period" .= longPeriod
                        ]

                    -- Emit signal based on SMA relationship
                    let signal = if shortSma > longSma then "BULLISH" else "BEARISH"
                    metric "signal" $ object
                        [ "type" .= (signal :: String)
                        , "symbol" .= symbol
                        , "short_sma" .= roundTo shortSma 2
                        , "long_sma" .= roundTo longSma 2
                        ]

                    success $ "Analysis complete: " ++ signal

extractConfig :: Value -> (String, Int, Int)
extractConfig (Object obj) =
    let symbol = case KM.lookup "symbol" obj of
            Just (String s) -> filter (/= '"') $ show s
            _ -> "AAPL"
        shortPeriod = case KM.lookup "short_period" obj of
            Just (Number n) -> round (toRealFloat n)
            _ -> 5
        longPeriod = case KM.lookup "long_period" obj of
            Just (Number n) -> round (toRealFloat n)
            _ -> 20
    in (symbol, shortPeriod, longPeriod)
extractConfig _ = ("AAPL", 5, 20)

fetchYahooFinance :: String -> IO (Either String [Double])
fetchYahooFinance symbol = do
    let url = "https://query1.finance.yahoo.com/v8/finance/chart/" ++ symbol ++ "?interval=1d&range=1mo"
    result <- try $ do
        request <- parseRequest url
        let requestWithUA = setRequestHeader "User-Agent" ["the0-sma-bot/1.0"] request
        response <- httpLBS requestWithUA
        return $ getResponseBody response
    case result of
        Left (e :: SomeException) -> return $ Left $ show e
        Right body -> return $ parsePrices body

parsePrices :: BL.ByteString -> Either String [Double]
parsePrices body = case eitherDecode body of
    Left err -> Left err
    Right json -> extractPrices json
  where
    eitherDecode = Data.Aeson.eitherDecode

extractPrices :: Value -> Either String [Double]
extractPrices (Object obj) = do
    chart <- maybe (Left "Missing chart") Right $ KM.lookup "chart" obj
    case chart of
        Object chartObj -> do
            resultArr <- maybe (Left "Missing result") Right $ KM.lookup "result" chartObj
            case resultArr of
                Array arr | not (null arr) ->
                    case head (toList arr) of
                        Object resultObj -> do
                            indicators <- maybe (Left "Missing indicators") Right $ KM.lookup "indicators" resultObj
                            case indicators of
                                Object indObj -> do
                                    quote <- maybe (Left "Missing quote") Right $ KM.lookup "quote" indObj
                                    case quote of
                                        Array quoteArr | not (null quoteArr) ->
                                            case head (toList quoteArr) of
                                                Object q -> do
                                                    closeData <- maybe (Left "Missing close") Right $ KM.lookup "close" q
                                                    case closeData of
                                                        Array closes -> Right $ map extractDouble (toList closes)
                                                        _ -> Left "close not array"
                                                _ -> Left "quote element not object"
                                        _ -> Left "quote not array"
                                _ -> Left "indicators not object"
                        _ -> Left "result element not object"
                _ -> Left "result not array"
        _ -> Left "chart not object"
  where
    toList (Array a) = toList a
    toList _ = []

extractDouble :: Value -> Double
extractDouble (Number n) = toRealFloat n
extractDouble _ = 0.0

calculateSma :: [Double] -> Int -> Double
calculateSma prices period
    | length prices < period = 0.0
    | otherwise = sum (take period (reverse prices)) / fromIntegral period

roundTo :: Double -> Int -> Double
roundTo value decimals =
    let multiplier = 10 ^ decimals
    in fromIntegral (round (value * multiplier) :: Integer) / multiplier
```

## SDK Functions

The Haskell SDK provides these functions in the `The0.Input` module:

### parse

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns a tuple of the bot ID and configuration as an aeson Value:

```haskell
(botId, config) <- parse
```

### metric

Emits a metric to the platform dashboard:

```haskell
metric "price" $ object
    [ "symbol" .= ("AAPL" :: String)
    , "value" .= (150.25 :: Double)
    ]
```

### success

Reports successful execution:

```haskell
success "Analysis complete"
```

### The0.Input.error

Reports failure and terminates with exit code 1:

```haskell
if null prices
    then The0.Input.error "No price data available"
    else processData prices
```

### result

Outputs a custom JSON result:

```haskell
result $ object
    [ "status" .= ("success" :: String)
    , "trade_id" .= ("abc123" :: String)
    ]
```

## Building

The CLI handles building automatically during deployment, but you can also build manually:

```bash
# Build and copy binaries to bin/
cabal build --enable-optimization=2
mkdir -p bin
cp $(cabal list-bin sma-bot) bin/
```

Or let the CLI handle everything:

```bash
the0 custom-bot deploy
```

The CLI builds your project with optimizations and copies all executables to `bin/` automatically.

## Testing Locally

Test by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20}'
export CODE_MOUNT_DIR="/tmp"

cabal run sma-bot
```

## Deploying

Deploy your compiled bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages the compiled binary along with configuration files and uploads everything. You must compile locally before deploying.

## Creating Bot Instances

Once deployed, create instances that run your bot on a schedule:

```json
{
  "name": "aapl-analysis",
  "type": "scheduled/sma-crossover",
  "version": "1.0.0",
  "schedule": "0 9 * * 1-5",
  "config": {
    "symbol": "AAPL",
    "short_period": 5,
    "long_period": 20
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

The bot will run at 9 AM UTC on weekdays.

## Monitoring

Monitor bot executions:

```bash
# List instances
the0 bot list

# View logs
the0 bot logs <bot_id>

# Delete instance
the0 bot delete <bot_id>
```

## Next Steps

With your first Haskell bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices

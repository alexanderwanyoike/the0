{-|
SMA Crossover Bot (Haskell)
===========================
A scheduled bot that implements Simple Moving Average crossover strategy
using live data from Yahoo Finance.

This example demonstrates:
- Fetching real market data from Yahoo Finance REST API
- Calculating Simple Moving Averages (SMA) with pure functions
- Detecting SMA crossovers for trading signals
- Structured metric emission for dashboard visualization

Metrics emitted:
- price: Current stock price with change percentage
- sma: Short and long SMA values
- signal: BUY/SELL signals when crossover detected

This is a SCHEDULED bot - it runs once per trigger, analyzes current data,
and exits with a result.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.HTTP.Simple
import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, catMaybes)
import GHC.Generics
import Text.Printf (printf)

import qualified The0.Input as Input

-- Configuration
data Config = Config
  { cfgSymbol :: String
  , cfgShortPeriod :: Int
  , cfgLongPeriod :: Int
  } deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .:? "symbol" .!= "AAPL"
    <*> v .:? "short_period" .!= 5
    <*> v .:? "long_period" .!= 20

-- Yahoo Finance response structure
data YahooResponse = YahooResponse
  { chart :: ChartData
  } deriving (Show, Generic)

instance FromJSON YahooResponse

data ChartData = ChartData
  { result :: Maybe [ChartResult]
  } deriving (Show, Generic)

instance FromJSON ChartData

data ChartResult = ChartResult
  { indicators :: Indicators
  } deriving (Show, Generic)

instance FromJSON ChartResult

data Indicators = Indicators
  { quote :: [Quote]
  } deriving (Show, Generic)

instance FromJSON Indicators

data Quote = Quote
  { close :: [Maybe Double]
  } deriving (Show, Generic)

instance FromJSON Quote

-- | Main entry point
main :: IO ()
main = do
  -- Get configuration using the0 SDK
  (botId, configVal) <- Input.parse

  let config = fromMaybe (Config "AAPL" 5 20) $
        case configVal of
          Object _ -> decode (encode configVal)
          _ -> Nothing

  Input.log $ "Bot " ++ botId ++ " started - " ++ cfgSymbol config ++
              " SMA(" ++ show (cfgShortPeriod config) ++ "/" ++
              show (cfgLongPeriod config) ++ ")"

  -- Fetch and process data
  processResult <- processData config

  case processResult of
    Left err -> do
      Input.log $ "Error: " ++ err
      Input.error err
    Right msg -> do
      Input.log $ "Completed: " ++ msg
      Input.success msg

-- | Process market data and generate signals
processData :: Config -> IO (Either String String)
processData config = do
  pricesResult <- fetchYahooFinance (cfgSymbol config)

  case pricesResult of
    Left err -> return $ Left err
    Right prices -> do
      let shortPeriod = cfgShortPeriod config
          longPeriod = cfgLongPeriod config
          symbol = cfgSymbol config

      if length prices < longPeriod
        then do
          Input.log $ "Insufficient data: " ++ show (length prices) ++
                      "/" ++ show longPeriod ++ " required for " ++ symbol
          return $ Left "Insufficient data for analysis"
        else do
          -- Get current price
          let currentPrice = last prices
              previousPrice = if length prices > 1 then prices !! (length prices - 2) else currentPrice
              changePct = if previousPrice /= 0
                          then ((currentPrice - previousPrice) / previousPrice) * 100
                          else 0

          -- Emit price metric using SDK
          Input.metric "price" $ object
            [ fromText "symbol" .= symbol
            , fromText "value" .= roundTo 2 currentPrice
            , fromText "change_pct" .= roundTo 3 changePct
            ]

          -- Calculate SMAs
          let shortSma = calculateSMA prices shortPeriod
              longSma = calculateSMA prices longPeriod

          -- Emit SMA metric using SDK
          Input.metric "sma" $ object
            [ fromText "symbol" .= symbol
            , fromText "short_sma" .= roundTo 2 shortSma
            , fromText "long_sma" .= roundTo 2 longSma
            , fromText "short_period" .= shortPeriod
            , fromText "long_period" .= longPeriod
            ]

          -- Determine trend
          let trend = if shortSma > longSma then "bullish" else "bearish"
              confidence = min (abs (shortSma - longSma) / longSma * 100) 0.95
              signalType = if shortSma > longSma then "BUY" else "SELL"

          -- Emit signal metric using SDK
          Input.metric "signal" $ object
            [ fromText "type" .= signalType
            , fromText "symbol" .= symbol
            , fromText "price" .= roundTo 2 currentPrice
            , fromText "confidence" .= roundTo 2 confidence
            , fromText "reason" .=
                ("SMA" ++ show shortPeriod ++ " is " ++
                (if shortSma > longSma then "above" else "below") ++
                " SMA" ++ show longPeriod ++ " (" ++ trend ++ " trend)")
            ]

          return $ Right $ "Analyzed " ++ symbol ++ ": " ++ signalType ++
                          " recommendation (" ++ trend ++ ")"

-- | Fetch historical data from Yahoo Finance
fetchYahooFinance :: String -> IO (Either String [Double])
fetchYahooFinance symbol = do
  let url = "https://query1.finance.yahoo.com/v8/finance/chart/" ++
            symbol ++ "?interval=1d&range=1mo"

  request <- parseRequest url
  let request' = setRequestHeader "User-Agent" ["the0-sma-bot/1.0"] request

  response <- httpLBS request'

  let body = getResponseBody response
  case decode body :: Maybe YahooResponse of
    Nothing -> return $ Left "Failed to parse Yahoo Finance response"
    Just yahooResp ->
      case result (chart yahooResp) of
        Nothing -> return $ Left "No result in Yahoo Finance response"
        Just [] -> return $ Left "Empty result in Yahoo Finance response"
        Just (r:_) ->
          case quote (indicators r) of
            [] -> return $ Left "No quote data in response"
            (q:_) -> return $ Right $ catMaybes $ close q

-- | Calculate Simple Moving Average
calculateSMA :: [Double] -> Int -> Double
calculateSMA prices period
  | length prices < period = 0
  | otherwise = sum (takeLast period prices) / fromIntegral period
  where
    takeLast n xs = drop (length xs - n) xs

-- | Round to specified decimal places
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

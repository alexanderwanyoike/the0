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
import Data.Aeson.Types (Parser)
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, catMaybes)
import System.Environment (lookupEnv)
import GHC.Generics
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf (printf)

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
  -- Get configuration from environment
  botId <- fromMaybe "test-bot" <$> lookupEnv "BOT_ID"
  configJson <- fromMaybe "{}" <$> lookupEnv "BOT_CONFIG"

  let config = fromMaybe (Config "AAPL" 5 20) $
        decode (BL.fromStrict $ TE.encodeUtf8 $ T.pack configJson)

  emitLog "bot_started"
    [ ("botId", String $ T.pack botId)
    , ("symbol", String $ T.pack $ cfgSymbol config)
    , ("shortPeriod", Number $ fromIntegral $ cfgShortPeriod config)
    , ("longPeriod", Number $ fromIntegral $ cfgLongPeriod config)
    ]

  -- Fetch and process data
  processResult <- processData config

  case processResult of
    Left err -> do
      emitLog "error" [("message", String $ T.pack err)]
      emitResult "error" err
    Right msg -> do
      emitLog "bot_completed" [("message", String $ T.pack msg)]
      emitResult "success" msg

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
          emitLog "insufficient_data"
            [ ("symbol", String $ T.pack symbol)
            , ("required", Number $ fromIntegral longPeriod)
            , ("available", Number $ fromIntegral $ length prices)
            ]
          return $ Left "Insufficient data for analysis"
        else do
          -- Get current price
          let currentPrice = last prices
              previousPrice = if length prices > 1 then prices !! (length prices - 2) else currentPrice
              changePct = if previousPrice /= 0
                          then ((currentPrice - previousPrice) / previousPrice) * 100
                          else 0

          ts <- getCurrentTimestamp

          -- Emit price metric
          emitMetric "price"
            [ ("symbol", String $ T.pack symbol)
            , ("value", Number $ realToFrac $ roundTo 2 currentPrice)
            , ("change_pct", Number $ realToFrac $ roundTo 3 changePct)
            , ("timestamp", String $ T.pack ts)
            ]

          -- Calculate SMAs
          let shortSma = calculateSMA prices shortPeriod
              longSma = calculateSMA prices longPeriod

          -- Emit SMA metric
          emitMetric "sma"
            [ ("symbol", String $ T.pack symbol)
            , ("short_sma", Number $ realToFrac $ roundTo 2 shortSma)
            , ("long_sma", Number $ realToFrac $ roundTo 2 longSma)
            , ("short_period", Number $ fromIntegral shortPeriod)
            , ("long_period", Number $ fromIntegral longPeriod)
            ]

          -- Determine trend
          let trend = if shortSma > longSma then "bullish" else "bearish"
              confidence = min (abs (shortSma - longSma) / longSma * 100) 0.95
              signalType = if shortSma > longSma then "BUY" else "SELL"

          -- Emit signal metric
          emitMetric "signal"
            [ ("type", String $ T.pack signalType)
            , ("symbol", String $ T.pack symbol)
            , ("price", Number $ realToFrac $ roundTo 2 currentPrice)
            , ("confidence", Number $ realToFrac $ roundTo 2 confidence)
            , ("reason", String $ T.pack $
                "SMA" ++ show shortPeriod ++ " is " ++
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

-- | Emit a metric to stdout
emitMetric :: String -> [(T.Text, Value)] -> IO ()
emitMetric metricType fields = do
  let keyFields = map (\(k, v) -> (fromText k, v)) fields
  let output = object $ (fromText "_metric", String $ T.pack metricType) : keyFields
  BLC.putStrLn $ encode output

-- | Emit a log message to stdout
emitLog :: String -> [(T.Text, Value)] -> IO ()
emitLog event fields = do
  ts <- getCurrentTimestamp
  let keyFields = map (\(k, v) -> (fromText k, v)) fields
  let output = object $
        (fromText "event", String $ T.pack event) :
        (fromText "timestamp", String $ T.pack ts) : keyFields
  BLC.putStrLn $ encode output

-- | Emit final result
emitResult :: String -> String -> IO ()
emitResult status message = do
  let output = object
        [ (fromText "_result", String $ T.pack status)
        , (fromText "message", String $ T.pack message)
        ]
  BLC.putStrLn $ encode output

-- | Get current timestamp as string
getCurrentTimestamp :: IO String
getCurrentTimestamp = do
  t <- getPOSIXTime
  let secs = floor t :: Int
      millis = floor ((t - fromIntegral secs) * 1000) :: Int
  return $ show secs ++ "." ++ printf "%03d" millis ++ "Z"

-- | Round to specified decimal places
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral (round (x * 10^n) :: Integer) / 10^n

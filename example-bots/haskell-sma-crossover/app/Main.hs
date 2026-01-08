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
- Persistent state for SMA values across runs

Metrics emitted:
- price: Current stock price with change percentage
- sma: Short and long SMA values
- signal: BUY/SELL signals when crossover detected

State Usage:
- Persists previous SMA values for crossover detection across runs
- Tracks total signal count for monitoring

This is a SCHEDULED bot - it runs once per trigger, analyzes current data,
and exits with a result.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.HTTP.Simple
import Data.Aeson (FromJSON, ToJSON, Value(..), decode, encode, object, withObject, (.=), (.:?), (.!=))
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
import qualified The0.State as State

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

-- Persistent state for crossover detection
data PersistedState = PersistedState
  { psPrevShortSma :: Maybe Double
  , psPrevLongSma :: Maybe Double
  , psSignalCount :: Int
  } deriving (Show, Generic)

instance FromJSON PersistedState where
  parseJSON = withObject "PersistedState" $ \v -> PersistedState
    <$> v .:? "prev_short_sma"
    <*> v .:? "prev_long_sma"
    <*> v .:? "signal_count" .!= 0

instance ToJSON PersistedState where
  toJSON s = object
    [ fromText "prev_short_sma" .= psPrevShortSma s
    , fromText "prev_long_sma" .= psPrevLongSma s
    , fromText "signal_count" .= psSignalCount s
    ]

defaultState :: PersistedState
defaultState = PersistedState Nothing Nothing 0

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

  -- Load persistent state from previous runs
  persistedState <- fromMaybe defaultState <$> State.get "bot_state"

  Input.log $ "Bot " ++ botId ++ " started - " ++ cfgSymbol config ++
              " SMA(" ++ show (cfgShortPeriod config) ++ "/" ++
              show (cfgLongPeriod config) ++ ") - loaded " ++
              show (psSignalCount persistedState) ++ " signals"

  -- Fetch and process data
  processResult <- processData config persistedState

  case processResult of
    Left err -> do
      Input.log $ "Error: " ++ err
      Input.error err
    Right msg -> do
      Input.log $ "Completed: " ++ msg
      Input.success msg

-- | Process market data and generate signals
processData :: Config -> PersistedState -> IO (Either String String)
processData config persistedState = do
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

          -- Check for crossover signal using previous SMA values from state
          let prevShort = psPrevShortSma persistedState
              prevLong = psPrevLongSma persistedState
              currentSignalCount = psSignalCount persistedState
              crossoverSignal = checkCrossover prevShort prevLong shortSma longSma
              newSignalCount = if crossoverSignal /= Nothing
                               then currentSignalCount + 1
                               else currentSignalCount

          -- Emit signal if crossover detected
          case crossoverSignal of
            Just signalType -> do
              let confidence = min (abs (shortSma - longSma) / longSma) 0.95
                  direction = if signalType == "BUY" then "above" else "below"
              Input.metric "signal" $ object
                [ fromText "type" .= signalType
                , fromText "symbol" .= symbol
                , fromText "price" .= roundTo 2 currentPrice
                , fromText "confidence" .= roundTo 2 confidence
                , fromText "total_signals" .= newSignalCount
                , fromText "reason" .=
                    ("SMA" ++ show shortPeriod ++ " crossed " ++ direction ++
                     " SMA" ++ show longPeriod)
                ]
            Nothing -> return ()

          -- Save state for next run
          let newState = PersistedState
                { psPrevShortSma = Just shortSma
                , psPrevLongSma = Just longSma
                , psSignalCount = newSignalCount
                }
          State.set "bot_state" newState

          let trend = if shortSma > longSma then "bullish" else "bearish"
              signalStr = case crossoverSignal of
                Just s -> s ++ " signal"
                Nothing -> "no crossover"
          return $ Right $ "Analyzed " ++ symbol ++ ": " ++ signalStr ++
                          " (" ++ trend ++ " trend, " ++
                          show newSignalCount ++ " total signals)"

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

-- | Check for SMA crossover
-- Returns Just "BUY" for golden cross (short crosses above long)
-- Returns Just "SELL" for death cross (short crosses below long)
-- Returns Nothing if no crossover or insufficient history
checkCrossover :: Maybe Double -> Maybe Double -> Double -> Double -> Maybe String
checkCrossover (Just prevShort) (Just prevLong) currShort currLong
  -- Golden cross: short SMA crosses above long SMA
  | prevShort <= prevLong && currShort > currLong = Just "BUY"
  -- Death cross: short SMA crosses below long SMA
  | prevShort >= prevLong && currShort < currLong = Just "SELL"
  | otherwise = Nothing
checkCrossover _ _ _ _ = Nothing

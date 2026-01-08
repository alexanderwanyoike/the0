{-|
SMA Crossover Query Handlers (Haskell)
======================================
Query endpoints for the SMA crossover bot.

These handlers provide read-only access to bot state and computed data.
They can be executed via:
  - CLI: the0 bot query <bot_id> /status
  - API: POST /bot/:id/query

Available queries:
  /status     - Get current SMA values and configuration
  /signals    - Get signal count
  /statistics - Get overall bot statistics
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified The0.Query as Query
import qualified The0.State as State
import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Aeson.Key (fromText)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Text.Read (readMaybe)

-- Persistent state (matches Main.hs)
data PersistedState = PersistedState
  { psPrevShortSma :: Maybe Double
  , psPrevLongSma :: Maybe Double
  , psSignalCount :: Int
  } deriving (Show, Generic)

instance FromJSON PersistedState
instance ToJSON PersistedState

defaultState :: PersistedState
defaultState = PersistedState Nothing Nothing 0

main :: IO ()
main = do
    -- /status - Get bot status and current SMA values
    Query.handler "/status" $ \req -> do
        persisted <- fromMaybe defaultState <$> State.get "bot_state"
        return $ object
            [ fromText "prev_short_sma" .= psPrevShortSma persisted
            , fromText "prev_long_sma" .= psPrevLongSma persisted
            , fromText "signal_count" .= psSignalCount persisted
            ]

    -- /signals - Get signal statistics
    Query.handler "/signals" $ \req -> do
        let limitStr = Query.getParam req "limit" "10"
            limit = fromMaybe 10 (readMaybe limitStr :: Maybe Int)
        persisted <- fromMaybe defaultState <$> State.get "bot_state"
        return $ object
            [ fromText "signal_count" .= psSignalCount persisted
            , fromText "limit_applied" .= limit
            ]

    -- /statistics - Get overall statistics
    Query.handler "/statistics" $ \req -> do
        persisted <- fromMaybe defaultState <$> State.get "bot_state"
        return $ object
            [ fromText "total_signals" .= psSignalCount persisted
            , fromText "has_data" .= case (psPrevShortSma persisted, psPrevLongSma persisted) of
                (Just _, Just _) -> True
                _ -> False
            ]

    Query.run

{-# LANGUAGE OverloadedStrings #-}

module StateSpec (spec) where

import Test.Hspec
import The0.State as State
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (try, SomeException, bracket)

-- | Helper to run tests with a temporary state directory
withTempStateDir :: (FilePath -> IO a) -> IO a
withTempStateDir action = withSystemTempDirectory "the0-state-test" $ \tempDir -> do
    originalStateDir <- lookupEnv "STATE_DIR"
    setEnv "STATE_DIR" tempDir
    result <- action tempDir
    case originalStateDir of
        Just dir -> setEnv "STATE_DIR" dir
        Nothing -> unsetEnv "STATE_DIR"
    return result

spec :: Spec
spec = do
    describe "The0.State.set and get" $ do
        it "stores and retrieves an object" $ withTempStateDir $ \_ -> do
            let portfolio = object ["AAPL" .= (100 :: Int), "GOOGL" .= (50 :: Int)]
            State.set "portfolio" portfolio
            retrieved <- State.get "portfolio" :: IO (Maybe Value)
            retrieved `shouldBe` Just portfolio

        it "stores and retrieves an array" $ withTempStateDir $ \_ -> do
            let prices = [45000.5, 45100.0, 45050.25] :: [Double]
            State.set "prices" prices
            retrieved <- State.get "prices" :: IO (Maybe [Double])
            retrieved `shouldBe` Just prices

        it "stores and retrieves a number" $ withTempStateDir $ \_ -> do
            State.set "count" (42 :: Int)
            retrieved <- State.get "count" :: IO (Maybe Int)
            retrieved `shouldBe` Just 42

        it "stores and retrieves a string" $ withTempStateDir $ \_ -> do
            State.set "symbol" ("BTC/USD" :: String)
            retrieved <- State.get "symbol" :: IO (Maybe String)
            retrieved `shouldBe` Just "BTC/USD"

        it "stores and retrieves a boolean" $ withTempStateDir $ \_ -> do
            State.set "active" True
            retrieved <- State.get "active" :: IO (Maybe Bool)
            retrieved `shouldBe` Just True

    describe "The0.State.get with defaults" $ do
        it "returns Nothing for non-existent key" $ withTempStateDir $ \_ -> do
            retrieved <- State.get "nonexistent" :: IO (Maybe Int)
            retrieved `shouldBe` Nothing

    describe "The0.State.exists" $ do
        it "returns True for existing key" $ withTempStateDir $ \_ -> do
            State.set "exists_test" ("value" :: String)
            result <- State.exists "exists_test"
            result `shouldBe` True

        it "returns False for non-existent key" $ withTempStateDir $ \_ -> do
            result <- State.exists "nonexistent"
            result `shouldBe` False

    describe "The0.State.delete" $ do
        it "removes existing key" $ withTempStateDir $ \_ -> do
            State.set "to_delete" ("value" :: String)
            existsBefore <- State.exists "to_delete"
            existsBefore `shouldBe` True

            result <- State.delete "to_delete"
            result `shouldBe` True

            existsAfter <- State.exists "to_delete"
            existsAfter `shouldBe` False

        it "returns False for non-existent key" $ withTempStateDir $ \_ -> do
            result <- State.delete "nonexistent"
            result `shouldBe` False

    describe "The0.State.list" $ do
        it "returns all keys" $ withTempStateDir $ \_ -> do
            State.set "key1" ("value1" :: String)
            State.set "key2" ("value2" :: String)
            State.set "key3" ("value3" :: String)

            keys <- State.list
            length keys `shouldBe` 3
            -- Check all keys exist (order may vary)
            keys `shouldContain` ["key1"]
            keys `shouldContain` ["key2"]
            keys `shouldContain` ["key3"]

        it "returns empty list when state is empty" $ withTempStateDir $ \tempDir -> do
            -- Ensure dir exists but is empty
            createDirectoryIfMissing True tempDir
            keys <- State.list
            keys `shouldBe` []

    describe "The0.State.clear" $ do
        it "removes all state" $ withTempStateDir $ \_ -> do
            State.set "key1" ("value1" :: String)
            State.set "key2" ("value2" :: String)

            keysBefore <- State.list
            length keysBefore `shouldBe` 2

            State.clear

            keysAfter <- State.list
            keysAfter `shouldBe` []

        it "does not throw when state is already empty" $ withTempStateDir $ \tempDir -> do
            createDirectoryIfMissing True tempDir
            -- Should not throw
            State.clear
            keys <- State.list
            keys `shouldBe` []

    describe "The0.State key validation" $ do
        it "throws for empty key" $ withTempStateDir $ \_ -> do
            result <- try (State.get "" :: IO (Maybe String)) :: IO (Either SomeException (Maybe String))
            case result of
                Left _ -> return ()  -- Expected
                Right _ -> expectationFailure "Should throw for empty key"

        it "throws for key with forward slash" $ withTempStateDir $ \_ -> do
            result <- try (State.set "../escape" ("evil" :: String)) :: IO (Either SomeException ())
            case result of
                Left _ -> return ()  -- Expected
                Right _ -> expectationFailure "Should throw for key with forward slash"

        it "throws for key with backslash" $ withTempStateDir $ \_ -> do
            result <- try (State.set "..\\escape" ("evil" :: String)) :: IO (Either SomeException ())
            case result of
                Left _ -> return ()  -- Expected
                Right _ -> expectationFailure "Should throw for key with backslash"

        it "throws for key with double dots" $ withTempStateDir $ \_ -> do
            result <- try (State.set ".." ("evil" :: String)) :: IO (Either SomeException ())
            case result of
                Left _ -> return ()  -- Expected
                Right _ -> expectationFailure "Should throw for key with double dots"

    describe "The0.State complex data" $ do
        it "handles complex nested structures" $ withTempStateDir $ \_ -> do
            let complexData = object
                    [ "portfolio" .= object
                        [ "holdings" .= [ object ["symbol" .= ("AAPL" :: String), "quantity" .= (100 :: Int), "price" .= (150.25 :: Double)]
                                        , object ["symbol" .= ("GOOGL" :: String), "quantity" .= (50 :: Int), "price" .= (2800.0 :: Double)]
                                        ]
                        , "total_value" .= (155025.0 :: Double)
                        ]
                    , "signals" .= [ object ["type" .= ("BUY" :: String), "symbol" .= ("AAPL" :: String), "confidence" .= (0.85 :: Double)]
                                   , object ["type" .= ("SELL" :: String), "symbol" .= ("TSLA" :: String), "confidence" .= (0.72 :: Double)]
                                   ]
                    , "metadata" .= object
                        [ "last_update" .= ("2024-01-15T10:30:00Z" :: String)
                        , "version" .= (2 :: Int)
                        ]
                    ]

            State.set "complex" complexData
            retrieved <- State.get "complex" :: IO (Maybe Value)
            retrieved `shouldBe` Just complexData

        it "overwrites existing key" $ withTempStateDir $ \_ -> do
            State.set "key" ("original" :: String)
            retrieved1 <- State.get "key" :: IO (Maybe String)
            retrieved1 `shouldBe` Just "original"

            State.set "key" ("updated" :: String)
            retrieved2 <- State.get "key" :: IO (Maybe String)
            retrieved2 `shouldBe` Just "updated"

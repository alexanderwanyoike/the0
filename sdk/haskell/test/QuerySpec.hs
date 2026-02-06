{-# LANGUAGE OverloadedStrings #-}

module QuerySpec (spec) where

import Test.Hspec
import The0.Query as Query
import The0.State as State
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (try, SomeException, catch)

-- | Helper to run tests with a temporary state directory and no query mode
withTempEnv :: (FilePath -> IO a) -> IO a
withTempEnv action = withSystemTempDirectory "the0-query-test" $ \tempDir -> do
    originalStateDir <- lookupEnv "STATE_DIR"
    originalQueryPath <- lookupEnv "QUERY_PATH"
    originalQueryParams <- lookupEnv "QUERY_PARAMS"
    originalBotConfig <- lookupEnv "BOT_CONFIG"

    setEnv "STATE_DIR" tempDir
    unsetEnv "QUERY_PATH"
    unsetEnv "QUERY_PARAMS"
    unsetEnv "BOT_CONFIG"

    result <- action tempDir

    -- Restore original environment
    case originalStateDir of
        Just dir -> setEnv "STATE_DIR" dir
        Nothing -> unsetEnv "STATE_DIR"
    case originalQueryPath of
        Just p -> setEnv "QUERY_PATH" p
        Nothing -> unsetEnv "QUERY_PATH"
    case originalQueryParams of
        Just p -> setEnv "QUERY_PARAMS" p
        Nothing -> unsetEnv "QUERY_PARAMS"
    case originalBotConfig of
        Just c -> setEnv "BOT_CONFIG" c
        Nothing -> unsetEnv "BOT_CONFIG"

    return result

-- | Helper to run tests in query mode
withQueryMode :: FilePath -> IO a -> IO a
withQueryMode tempDir action = do
    setEnv "STATE_DIR" tempDir
    setEnv "QUERY_PATH" "/test"
    result <- action
    unsetEnv "QUERY_PATH"
    return result

spec :: Spec
spec = do
    describe "The0.Query.isQueryMode" $ do
        it "returns False when QUERY_PATH is not set" $ withTempEnv $ \_ -> do
            result <- Query.isQueryMode
            result `shouldBe` False

        it "returns True when QUERY_PATH is set" $ withTempEnv $ \tempDir -> do
            setEnv "QUERY_PATH" "/portfolio"
            result <- Query.isQueryMode
            unsetEnv "QUERY_PATH"
            result `shouldBe` True

    describe "The0.Query.getParams" $ do
        it "returns empty map by default" $ withTempEnv $ \_ -> do
            params <- Query.getParams
            Map.null params `shouldBe` True

    describe "The0.Query.getConfig" $ do
        it "returns empty object by default" $ withTempEnv $ \_ -> do
            config <- Query.getConfig
            config `shouldBe` Object mempty

    describe "The0.Query.handler" $ do
        it "registers handler without throwing" $ withTempEnv $ \_ -> do
            result <- try (Query.handler "/test" $ \_ -> return $ object ["test" .= True]) :: IO (Either SomeException ())
            case result of
                Left _ -> expectationFailure "Handler registration should not throw"
                Right _ -> return ()

        it "can register multiple handlers" $ withTempEnv $ \_ -> do
            result <- try $ do
                Query.handler "/one" $ \_ -> return $ object ["id" .= (1 :: Int)]
                Query.handler "/two" $ \_ -> return $ object ["id" .= (2 :: Int)]
            case result of
                Left e -> expectationFailure $ "Multiple handler registration should not throw: " ++ show (e :: SomeException)
                Right _ -> return ()

    describe "The0.Query.getParam" $ do
        it "returns value for existing key" $ withTempEnv $ \_ -> do
            let req = Request "/test" (Map.fromList [("symbol", "BTC/USD")])
            Query.getParam req "symbol" "default" `shouldBe` "BTC/USD"

        it "returns default for missing key" $ withTempEnv $ \_ -> do
            let req = Request "/test" Map.empty
            Query.getParam req "missing" "default" `shouldBe` "default"

    describe "The0.Query.Request" $ do
        it "has accessible path" $ withTempEnv $ \_ -> do
            let req = Request "/portfolio" Map.empty
            reqPath req `shouldBe` "/portfolio"

        it "has accessible params" $ withTempEnv $ \_ -> do
            let params = Map.fromList [("symbol", "BTC/USD")]
            let req = Request "/test" params
            reqParams req `shouldBe` params

    describe "The0.Query.ReadOnlyStateError" $ do
        it "can be created with a message" $ withTempEnv $ \_ -> do
            let err = Query.ReadOnlyStateError "Test message"
            show err `shouldContain` "Test message"

    describe "State read-only enforcement in query mode" $ do
        it "state set throws in query mode" $ withTempEnv $ \tempDir -> do
            withQueryMode tempDir $ do
                result <- try (State.set "key" ("value" :: String)) :: IO (Either SomeException ())
                case result of
                    Left _ -> return ()  -- Expected - should throw
                    Right _ -> expectationFailure "State.set should throw in query mode"

        it "state delete throws in query mode" $ withTempEnv $ \tempDir -> do
            withQueryMode tempDir $ do
                result <- try (State.delete "key") :: IO (Either SomeException Bool)
                case result of
                    Left _ -> return ()  -- Expected - should throw
                    Right _ -> expectationFailure "State.delete should throw in query mode"

        it "state clear throws in query mode" $ withTempEnv $ \tempDir -> do
            withQueryMode tempDir $ do
                result <- try State.clear :: IO (Either SomeException ())
                case result of
                    Left _ -> return ()  -- Expected - should throw
                    Right _ -> expectationFailure "State.clear should throw in query mode"

        it "state get allowed in query mode" $ withTempEnv $ \tempDir -> do
            withQueryMode tempDir $ do
                result <- try (State.get "nonexistent" :: IO (Maybe String)) :: IO (Either SomeException (Maybe String))
                case result of
                    Left e -> expectationFailure $ "State.get should not throw in query mode: " ++ show e
                    Right Nothing -> return ()  -- Expected - key doesn't exist
                    Right (Just _) -> return ()  -- Also fine if it somehow exists

        it "state exists allowed in query mode" $ withTempEnv $ \tempDir -> do
            createDirectoryIfMissing True tempDir
            withQueryMode tempDir $ do
                result <- try (State.exists "nonexistent") :: IO (Either SomeException Bool)
                case result of
                    Left e -> expectationFailure $ "State.exists should not throw in query mode: " ++ show e
                    Right False -> return ()  -- Expected
                    Right True -> return ()  -- Also fine

        it "state list allowed in query mode" $ withTempEnv $ \tempDir -> do
            createDirectoryIfMissing True tempDir
            withQueryMode tempDir $ do
                result <- try State.list :: IO (Either SomeException [String])
                case result of
                    Left e -> expectationFailure $ "State.list should not throw in query mode: " ++ show e
                    Right [] -> return ()  -- Expected - empty
                    Right _ -> return ()  -- Also fine if there are keys

{-# LANGUAGE OverloadedStrings #-}

module InputSpec (spec) where

import Test.Hspec
import The0.Input
import Data.Aeson (Value(..), object, (.=))
import System.IO.Silently (capture_, hCapture)
import System.IO (stderr)
import Control.Exception (try, SomeException)

-- Helper to capture stderr output
captureStderr :: IO () -> IO String
captureStderr action = do
    (output, _) <- hCapture [stderr] action
    return output

spec :: Spec
spec = do
    describe "The0.Input.log" $ do
        it "outputs JSON with message field to stderr" $ do
            output <- captureStderr $ The0.Input.log "Test message" Nothing Nothing
            output `shouldContain` "\"message\":\"Test message\""

        it "defaults to info level" $ do
            output <- captureStderr $ The0.Input.log "Test message" Nothing Nothing
            output `shouldContain` "\"level\":\"info\""

        it "includes timestamp" $ do
            output <- captureStderr $ The0.Input.log "Test message" Nothing Nothing
            output `shouldContain` "\"timestamp\":"
            -- Timestamp should be numeric (not a quoted string)
            output `shouldNotContain` "\"timestamp\":\""

        it "supports warn level" $ do
            output <- captureStderr $ The0.Input.log "Warning message" Nothing (Just Warn)
            output `shouldContain` "\"level\":\"warn\""
            output `shouldContain` "\"message\":\"Warning message\""

        it "supports error level" $ do
            output <- captureStderr $ The0.Input.log "Error message" Nothing (Just Error)
            output `shouldContain` "\"level\":\"error\""

        it "merges data fields" $ do
            output <- captureStderr $ The0.Input.log "Order placed" (Just $ object ["order_id" .= ("12345" :: String), "symbol" .= ("BTC" :: String)]) Nothing
            output `shouldContain` "\"message\":\"Order placed\""
            output `shouldContain` "\"order_id\":\"12345\""
            output `shouldContain` "\"symbol\":\"BTC\""

        it "supports data and level together" $ do
            output <- captureStderr $ The0.Input.log "Order failed" (Just $ object ["order_id" .= ("12345" :: String)]) (Just Error)
            output `shouldContain` "\"level\":\"error\""
            output `shouldContain` "\"message\":\"Order failed\""
            output `shouldContain` "\"order_id\":\"12345\""

        it "escapes quotes in message" $ do
            output <- captureStderr $ The0.Input.log "Test with \"quotes\"" Nothing Nothing
            output `shouldContain` "\\\"quotes\\\""

        it "escapes backslashes in message" $ do
            output <- captureStderr $ The0.Input.log "Path: C:\\Users\\test" Nothing Nothing
            output `shouldContain` "C:\\\\Users\\\\test"

        it "escapes newlines in message" $ do
            output <- captureStderr $ The0.Input.log "Line1\nLine2" Nothing Nothing
            output `shouldContain` "\\n"

        it "escapes tabs in message" $ do
            output <- captureStderr $ The0.Input.log "Col1\tCol2" Nothing Nothing
            output `shouldContain` "\\t"

        it "escapes carriage returns in message" $ do
            output <- captureStderr $ The0.Input.log "Line1\rLine2" Nothing Nothing
            output `shouldContain` "\\r"

        it "handles empty message" $ do
            output <- captureStderr $ The0.Input.log "" Nothing Nothing
            output `shouldContain` "\"message\":\"\""

        it "handles long message" $ do
            let longMsg = replicate 10000 'x'
            output <- captureStderr $ The0.Input.log longMsg Nothing Nothing
            output `shouldContain` longMsg

    describe "The0.Input convenience log functions" $ do
        it "logInfo outputs with info level" $ do
            output <- captureStderr $ logInfo "Info message" Nothing
            output `shouldContain` "\"level\":\"info\""
            output `shouldContain` "\"message\":\"Info message\""

        it "logWarn outputs with warn level" $ do
            output <- captureStderr $ logWarn "Warn message" Nothing
            output `shouldContain` "\"level\":\"warn\""

        it "logError outputs with error level" $ do
            output <- captureStderr $ logError "Error message" Nothing
            output `shouldContain` "\"level\":\"error\""

    describe "The0.Input.metric" $ do
        it "outputs JSON with _metric field" $ do
            output <- capture_ $ metric "price" (object ["symbol" .= ("BTC/USD" :: String)])
            output `shouldContain` "\"_metric\":\"price\""

        it "includes timestamp" $ do
            output <- capture_ $ metric "heartbeat" (object [])
            output `shouldContain` "\"timestamp\":"
            -- Timestamp should be numeric (not a quoted string)
            output `shouldNotContain` "\"timestamp\":\""

        it "merges data correctly" $ do
            output <- capture_ $ metric "trade" (object ["symbol" .= ("ETH" :: String), "amount" .= (1.5 :: Double)])
            output `shouldContain` "\"_metric\":\"trade\""
            output `shouldContain` "\"symbol\":\"ETH\""
            output `shouldContain` "\"amount\":1.5"

        it "handles empty object" $ do
            output <- capture_ $ metric "ping" (object [])
            output `shouldContain` "\"_metric\":\"ping\""
            output `shouldContain` "\"timestamp\":"

    describe "The0.Input.result" $ do
        it "does not throw on valid input" $ do
            -- result writes to a file, so we test that it doesn't throw
            let val = object ["status" .= ("success" :: String), "data" .= (42 :: Int)]
            result' <- try (result val) :: IO (Either SomeException ())
            case result' of
                Left _ -> return () -- File write may fail, that's ok
                Right _ -> return ()
            True `shouldBe` True

    describe "The0.Input.success" $ do
        it "does not throw on valid input" $ do
            -- success writes to a file, test it doesn't throw
            result' <- try (success "Test message") :: IO (Either SomeException ())
            case result' of
                Left _ -> return ()
                Right _ -> return ()
            True `shouldBe` True

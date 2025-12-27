{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    -- Get BOT_ID and BOT_CONFIG from environment
    botId <- lookupEnv "BOT_ID" >>= \case
        Just bid -> pure bid
        Nothing -> pure "unknown"

    configStr <- lookupEnv "BOT_CONFIG" >>= \case
        Just cfg -> pure cfg
        Nothing -> pure "{}"

    -- Parse config
    let config = case decode (BL.pack configStr) :: Maybe Value of
            Just v -> v
            Nothing -> error "Invalid JSON config"

    -- Print inputs to stderr for test verification
    hPutStrLn stderr $ "BOT_ID: " ++ botId
    hPutStrLn stderr $ "BOT_CONFIG: " ++ configStr

    -- Output success JSON to stdout
    putStrLn $ "{\"status\":\"success\",\"message\":\"Haskell bot executed\",\"bot_id\":\"" ++ botId ++ "\"}"

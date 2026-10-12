{-# LANGUAGE OverloadedStrings #-}

-- | A bot with handlers only Haskell can write, run through 'Query.run' in
-- both query modes:
--
-- * @/echo@ reads params the way Haskell bots did before params had JSON
--   types, with 'Query.getParam' and 'Query.reqParams', to show such bots
--   keep working.
-- * @/lazy-error@ returns a result that fails only once it is evaluated, as
--   pure code calling @error@, @head []@ or @fromJust Nothing@ does.
-- * @/metric@ and @/log@ print a line while the query server keeps running,
--   as a realtime bot does. GHC block-buffers stdout on a pipe, the bot
--   block-buffers stderr as any bot may, and the server stops on SIGTERM,
--   which flushes nothing.
module HaskellBot
    ( haskellBotArg
    , main
    , spec
    ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Harness (runCase, sandboxQueryResult)
import System.Environment (getExecutablePath)
import System.IO (BufferMode(..), hSetBuffering, stderr)
import Test.Hspec
import qualified The0.Input as Input
import qualified The0.Query as Query

haskellBotArg :: String
haskellBotArg = "haskell-bot"

main :: FilePath -> IO ()
main queryResultPath = do
    hSetBuffering stderr (BlockBuffering Nothing)
    Query.setResultPath queryResultPath
    Query.handler "/echo" $ \req -> return $ object
        [ "symbol" .= Query.getParam req "symbol" "none"
        , "limit" .= Query.getParam req "limit" "10"
        , "filter" .= Map.lookup "filter" (Query.reqParams req)
        , "cursor" .= Map.lookup "cursor" (Query.reqParams req)
        , "offset" .= Query.getParam req "offset" "0"
        ]
    Query.handler "/lazy-error" $ \_ -> return $ object
        ["price" .= (Prelude.error "price feed unavailable" :: Double)]
    Query.handler "/metric" $ \_ -> do
        Input.metric "price" (object ["value" .= (45000.5 :: Double)])
        return (object [])
    Query.handler "/log" $ \_ -> do
        Input.logInfo "Prices checked" Nothing
        return (object [])
    Query.run

spec :: Spec
spec = do
    describe "a bot reading text params" $ do
        it "reads every param as text in an ephemeral query" $ check ephemeralCase
        it "reads every param as text in the query server" $ check serverCase

    describe "a handler whose result fails when evaluated" $ do
        it "answers its message in an ephemeral query" $ check lazyErrorEphemeralCase
        it "answers 500 with its message in the query server, which keeps serving" $ check lazyErrorServerCase

    describe "a query server that prints lines" $ do
        it "flushes a metric line as soon as it prints it" $ check metricCase
        it "flushes a log line as soon as it prints it" $ check logCase

check :: Value -> Expectation
check case_ = do
    exe <- getExecutablePath
    (problems, output) <- runCase case_ exe $ \sandbox ->
        [haskellBotArg, sandboxQueryResult sandbox]
    if null problems then pure () else expectationFailure (unlines problems ++ output)

params :: Value
params = object
    [ "symbol" .= ("BTC/USD" :: String)
    , "limit" .= (5 :: Int)
    , "filter" .= object ["side" .= ("buy" :: String)]
    , "cursor" .= Aeson.Null
    ]

-- | What the handler answers for 'params': every value as text, a null as
-- absent.
textParams :: Value
textParams = object
    [ "symbol" .= ("BTC/USD" :: String)
    , "limit" .= ("5" :: String)
    , "filter" .= ("{\"side\":\"buy\"}" :: String)
    , "cursor" .= Aeson.Null
    , "offset" .= ("0" :: String)
    ]

env :: [(Aeson.Key, Value)] -> Value
env extra = object $
    [ "BOT_ID" .= ("haskell-bot" :: String)
    , "BOT_CONFIG" .= ("{}" :: String)
    , "CODE_MOUNT_DIR" .= ("{{code_mount_dir}}" :: String)
    , "STATE_DIR" .= ("{{state_dir}}" :: String)
    ] ++ [key .= value | (key, value) <- extra]

ephemeralCase :: Value
ephemeralCase = object
    [ "env" .= env
        [ ("QUERY_PATH", "/echo")
        , ("QUERY_PARAMS", Aeson.String (jsonText params))
        ]
    , "expect" .= object
        [ "exit_code" .= (0 :: Int)
        , "query_result" .= object ["status" .= ("ok" :: String), "data" .= textParams]
        ]
    ]

serverCase :: Value
serverCase = object
    [ "env" .= env [("BOT_TYPE", "realtime"), ("THE0_QUERY_PORT", "{{port}}")]
    , "requests" .=
        [ -- A runtime query: the params as the body, and copied into the
          -- query string as text, with the null left out.
          object
            [ "method" .= ("POST" :: String)
            , "target" .= ("/echo?filter=%7B%22side%22%3A%22buy%22%7D&limit=5&symbol=BTC%2FUSD" :: String)
            , "body" .= params
            , "expect" .= object
                [ "status" .= (200 :: Int)
                , "body" .= object ["status" .= ("ok" :: String), "data" .= textParams]
                ]
            ]
        , object
            [ "method" .= ("GET" :: String)
            , "target" .= ("/echo?symbol=BTC%2FUSD&limit=5" :: String)
            , "expect" .= object
                [ "status" .= (200 :: Int)
                , "body" .= object
                    [ "status" .= ("ok" :: String)
                    , "data" .= object
                        [ "symbol" .= ("BTC/USD" :: String)
                        , "limit" .= ("5" :: String)
                        , "filter" .= Aeson.Null
                        , "cursor" .= Aeson.Null
                        , "offset" .= ("0" :: String)
                        ]
                    ]
                ]
            ]
        ]
    ]

jsonText :: Value -> T.Text
jsonText = TE.decodeUtf8 . BL.toStrict . Aeson.encode

-- | The error's message alone, without the CallStack lines GHC adds to the
-- text of an @error@ call.
lazyError :: Value
lazyError = object
    [ "status" .= ("error" :: String)
    , "error" .= ("price feed unavailable" :: String)
    ]

lazyErrorEphemeralCase :: Value
lazyErrorEphemeralCase = object
    [ "env" .= env [("QUERY_PATH", "/lazy-error")]
    , "expect" .= object ["exit_code" .= (1 :: Int), "query_result" .= lazyError]
    ]

lazyErrorServerCase :: Value
lazyErrorServerCase = object
    [ "env" .= env [("BOT_TYPE", "realtime"), ("THE0_QUERY_PORT", "{{port}}")]
    , "requests" .=
        [ object
            [ "method" .= ("POST" :: String)
            , "target" .= ("/lazy-error" :: String)
            , "expect" .= object ["status" .= (500 :: Int), "body" .= lazyError]
            ]
        , object
            [ "method" .= ("GET" :: String)
            , "target" .= ("/health" :: String)
            , "expect" .= object
                [ "status" .= (200 :: Int)
                , "body" .= object
                    [ "status" .= ("ok" :: String)
                    , "data" .= object ["status" .= ("ok" :: String)]
                    ]
                ]
            ]
        ]
    ]

metricCase :: Value
metricCase = object
    [ "env" .= env [("BOT_TYPE", "realtime"), ("THE0_QUERY_PORT", "{{port}}")]
    , "requests" .=
        [ object
            [ "method" .= ("POST" :: String)
            , "target" .= ("/metric" :: String)
            , "expect" .= object
                [ "status" .= (200 :: Int)
                , "body" .= object ["status" .= ("ok" :: String), "data" .= object []]
                ]
            ]
        ]
    , "expect" .= object
        [ "stdout" .=
            [ object
                [ "_metric" .= ("price" :: String)
                , "value" .= (45000.5 :: Double)
                , "timestamp" .= object ["$match" .= ("timestamp" :: String)]
                ]
            ]
        ]
    ]

logCase :: Value
logCase = object
    [ "env" .= env [("BOT_TYPE", "realtime"), ("THE0_QUERY_PORT", "{{port}}")]
    , "requests" .=
        [ object
            [ "method" .= ("POST" :: String)
            , "target" .= ("/log" :: String)
            , "expect" .= object
                [ "status" .= (200 :: Int)
                , "body" .= object ["status" .= ("ok" :: String), "data" .= object []]
                ]
            ]
        ]
    , "expect" .= object
        [ "stderr" .=
            [ logLine "info" (object ["$match" .= ("string" :: String)])
            , logLine "info" ("Prices checked" :: String)
            ]
        ]
    ]
  where
    logLine :: Aeson.ToJSON a => String -> a -> Value
    logLine level message = object
        [ "level" .= level
        , "message" .= message
        , "timestamp" .= object ["$match" .= ("timestamp" :: String)]
        ]

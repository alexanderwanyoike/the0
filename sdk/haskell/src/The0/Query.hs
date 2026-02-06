{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : The0.Query
-- Description : Express-like handler interface for bot queries
-- License     : Apache-2.0
--
-- This module provides query handling capabilities for bots, allowing users to define
-- custom read-only query handlers that can be executed on demand.
--
-- Separate namespace from State and Input. Users explicitly import:
--
-- @
-- import The0.Query as Query
-- import The0.State as State  -- If needed in handlers
-- @
--
-- = Example
--
-- @
-- import The0.Query as Query
-- import The0.State as State
-- import Data.Aeson (object, (.=))
--
-- main :: IO ()
-- main = do
--     Query.handler "/portfolio" $ \req -> do
--         positions <- State.get "positions"
--         return $ object ["positions" .= positions, "count" .= (5 :: Int)]
--
--     Query.handler "/status" $ \req -> do
--         let symbol = Query.getParam req "symbol" "BTC/USD"
--         return $ object ["symbol" .= symbol, "active" .= True]
--
--     Query.run
-- @
module The0.Query
    ( handler
    , run
    , getParams
    , getConfig
    , isQueryMode
    , getParam
    , Request(..)
    , ReadOnlyStateError(..)
    ) where

import Control.Exception (Exception, catch, SomeException)
import Control.Monad (forM_, when)
import Data.Aeson (Value(..), decode, encode, object, (.=), ToJSON)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromString, toText)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Network.HTTP.Types (urlDecode)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- | Error thrown when attempting to modify state during query execution.
newtype ReadOnlyStateError = ReadOnlyStateError String
    deriving (Show, Eq)

instance Exception ReadOnlyStateError

-- | Request object passed to handlers (Express-like).
data Request = Request
    { reqPath   :: String
    , reqParams :: Map.Map String String
    } deriving (Show, Eq)

-- | Get a parameter value from the request.
getParam :: Request -> String -> String -> String
getParam req key def = fromMaybe def $ Map.lookup key (reqParams req)

-- | Handler function type
type Handler = Request -> IO Value

-- Global handler registry (using unsafePerformIO for simplicity)
{-# NOINLINE handlerRegistry #-}
handlerRegistry :: IORef (Map.Map String Handler)
handlerRegistry = unsafePerformIO $ newIORef Map.empty

-- Global current params
{-# NOINLINE currentParamsRef #-}
currentParamsRef :: IORef (Map.Map String String)
currentParamsRef = unsafePerformIO $ newIORef Map.empty

-- Global config
{-# NOINLINE configRef #-}
configRef :: IORef Value
configRef = unsafePerformIO $ newIORef (Object mempty)

-- | Register a query handler (Express-like).
--
-- @
-- Query.handler "/portfolio" $ \\req -> do
--     let symbol = Query.getParam req "symbol" "BTC/USD"
--     return $ object ["symbol" .= symbol, "positions" .= ([] :: [String])]
-- @
handler :: String -> Handler -> IO ()
handler path fn = modifyIORef' handlerRegistry (Map.insert path fn)

-- | Get current query parameters (alternative to request object).
getParams :: IO (Map.Map String String)
getParams = readIORef currentParamsRef

-- | Get the bot configuration.
getConfig :: IO Value
getConfig = readIORef configRef

-- | Check if currently running in query mode (read-only).
-- Used by State module to enforce read-only behavior.
isQueryMode :: IO Bool
isQueryMode = do
    mPath <- lookupEnv "QUERY_PATH"
    return $ case mPath of
        Just path | not (null path) -> True
        _ -> False

-- | Run the query system with automatic mode detection.
--
-- Modes:
-- - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
-- - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
-- - Neither: Info mode (print available handlers)
run :: IO ()
run = do
    -- Load bot config from environment
    mConfigStr <- lookupEnv "BOT_CONFIG"
    case mConfigStr of
        Just configStr -> case decode (BL.pack configStr) of
            Just val -> writeIORef configRef val
            Nothing -> writeIORef configRef (Object mempty)
        Nothing -> writeIORef configRef (Object mempty)

    -- Register built-in handlers
    handlers <- readIORef handlerRegistry
    when (not $ Map.member "/health" handlers) $
        handler "/health" $ \_ -> return $ object ["status" .= ("ok" :: String)]

    handlers' <- readIORef handlerRegistry
    when (not $ Map.member "/info" handlers') $
        handler "/info" $ \_ -> do
            hs <- readIORef handlerRegistry
            let paths = Map.keys hs
            return $ object ["available_queries" .= paths]

    mQueryPath <- lookupEnv "QUERY_PATH"
    mBotType <- lookupEnv "BOT_TYPE"

    case (mQueryPath, mBotType) of
        (Just path, _) | not (null path) -> runEphemeral path
        (_, Just "realtime") -> runServer
        _ -> runEphemeral "/info"

-- | Write query result to /query/result.json file (matches Python SDK behavior).
-- This avoids stdout pollution from runtime logs mixing with query results.
writeResult :: Value -> IO ()
writeResult result = do
    let resultPath = "/query/result.json"
    createDirectoryIfMissing True "/query"
    BL.writeFile resultPath (encode result)
        `catch` \(e :: SomeException) ->
            hPutStrLn stderr $ "RESULT_ERROR: Failed to write result file: " ++ show e

-- | Execute single query and write result to /query/result.json.
runEphemeral :: String -> IO ()
runEphemeral queryPath = do
    -- Parse parameters from environment
    mParamsStr <- lookupEnv "QUERY_PARAMS"
    case mParamsStr of
        Just paramsStr -> case decode (BL.pack paramsStr) of
            Just (Object o) -> do
                let params = Map.fromList [(T.unpack (toText k), valueToString v) | (k, v) <- KM.toList o]
                writeIORef currentParamsRef params
            _ -> writeIORef currentParamsRef Map.empty
        Nothing -> writeIORef currentParamsRef Map.empty

    -- Find and execute handler
    handlers <- readIORef handlerRegistry
    params <- readIORef currentParamsRef
    case Map.lookup queryPath handlers of
        Nothing -> do
            let available = Map.keys handlers
            writeResult $ object
                [ "status" .= ("error" :: String)
                , "error" .= ("No handler for path: " ++ queryPath)
                , "available" .= available
                ]
            exitWith (ExitFailure 1)

        Just fn -> do
            let req = Request queryPath params
            result <- (Right <$> fn req) `catch` handleError
            case result of
                Left err -> do
                    writeResult $ object
                        [ "status" .= ("error" :: String)
                        , "error" .= err
                        ]
                    exitWith (ExitFailure 1)
                Right val ->
                    writeResult $ object
                        [ "status" .= ("ok" :: String)
                        , "data" .= val
                        ]
  where
    handleError :: SomeException -> IO (Either String Value)
    handleError e = return $ Left (show e)

-- | Start HTTP server on port 9476 for realtime bots.
runServer :: IO ()
runServer = do
    mPortStr <- lookupEnv "THE0_QUERY_PORT"
    let port = case mPortStr of
            Just ps -> case reads ps of
                [(p, "")] -> p
                _ -> 9476
            Nothing -> 9476

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock 10

    hPutStrLn stderr $ BL.unpack $ encode $ object
        [ "_log" .= ("info" :: String)
        , "message" .= ("Query server started on port " ++ show port)
        ]

    serverLoop sock

serverLoop :: Socket -> IO ()
serverLoop sock = do
    (clientSock, _) <- accept sock
    handleConnection clientSock `catch` logError
    serverLoop sock
  where
    logError :: SomeException -> IO ()
    logError e = hPutStrLn stderr $ BL.unpack $ encode $ object
        [ "_log" .= ("error" :: String)
        , "message" .= ("Error handling request: " ++ show e)
        ]

handleConnection :: Socket -> IO ()
handleConnection sock = do
    request <- recv sock 4096
    let requestStr = BS.unpack request
        lines' = lines requestStr
    case lines' of
        (requestLine:_) -> do
            let parts = words requestLine
            case parts of
                (_method:pathWithQuery:_) -> do
                    let (path, params) = parsePathAndQuery pathWithQuery
                    writeIORef currentParamsRef params

                    handlers <- readIORef handlerRegistry
                    (status, body) <- case Map.lookup path handlers of
                        Nothing -> return
                            ( "404 Not Found"
                            , encode $ object
                                [ "status" .= ("error" :: String)
                                , "error" .= ("No handler for path: " ++ path)
                                ]
                            )
                        Just fn -> do
                            let req = Request path params
                            result <- (Right <$> fn req) `catch` (\e -> return $ Left (show (e :: SomeException)))
                            case result of
                                Left err -> return
                                    ( "500 Internal Server Error"
                                    , encode $ object
                                        [ "status" .= ("error" :: String)
                                        , "error" .= err
                                        ]
                                    )
                                Right val -> return
                                    ( "200 OK"
                                    , encode $ object
                                        [ "status" .= ("ok" :: String)
                                        , "data" .= val
                                        ]
                                    )

                    let bodyStr = BL.toStrict body
                        response = BS.pack $ unlines
                            [ "HTTP/1.1 " ++ status
                            , "Content-Type: application/json"
                            , "Content-Length: " ++ show (BS.length bodyStr)
                            , ""
                            ] ++ BS.unpack bodyStr
                    sendAll sock response
                _ -> return ()
        _ -> return ()

    close sock

-- | Parse path and query string from URL.
parsePathAndQuery :: String -> (String, Map.Map String String)
parsePathAndQuery pathWithQuery =
    case break (== '?') pathWithQuery of
        (path, "") -> (path, Map.empty)
        (path, '?':query) -> (path, parseQueryString query)
        _ -> (pathWithQuery, Map.empty)

-- | Parse query string into map.
parseQueryString :: String -> Map.Map String String
parseQueryString query =
    Map.fromList $ mapMaybe parsePair (splitOn '&' query)
  where
    parsePair pair = case break (== '=') pair of
        (key, '=':value) -> Just (urlDecodeString key, urlDecodeString value)
        (key, "") | not (null key) -> Just (urlDecodeString key, "")
        _ -> Nothing

    splitOn c s = case break (== c) s of
        (a, "") -> [a]
        (a, _:rest) -> a : splitOn c rest

    urlDecodeString = BS.unpack . urlDecode True . BS.pack

-- | Convert a Value to a String for simple cases.
valueToString :: Value -> String
valueToString (String t) = T.unpack t
valueToString v = BL.unpack (encode v)

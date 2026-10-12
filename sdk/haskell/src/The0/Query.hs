{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
-- import Data.Aeson (Value, object, (.=))
-- import Data.Maybe (fromMaybe)
--
-- main :: IO ()
-- main = do
--     Query.handler "/portfolio" $ \_ -> do
--         positions <- State.get "positions" :: IO (Maybe Value)
--         return $ object ["positions" .= positions]
--
--     Query.handler "/status" $ \req -> do
--         let symbol = Query.getParam req "symbol" "BTC/USD"
--             limit = fromMaybe 10 (Query.getParamAs req "limit") :: Int
--         return $ object ["symbol" .= symbol, "limit" .= limit]
--
--     Query.handler "/trade" $ \req ->
--         case Query.getParamAs req "id" :: Maybe Int of
--             Nothing -> fail "id is required"
--             Just tradeId -> return $ object ["id" .= tradeId]
--
--     Query.run
-- @
--
-- = Params
--
-- Params are JSON values. An ephemeral query reads them from @QUERY_PARAMS@;
-- the query server reads them from a JSON object body, or else from the URL
-- query string, where every value is a string.
--
-- * 'getParam' and 'reqParams' give a param as text: a string as it is, any
--   other value as its JSON text, and a @null@ as absent.
-- * 'getParamAs' and 'reqValues' keep the JSON types.
--
-- = Errors
--
-- A handler reports an error by throwing any exception, with 'fail' or
-- 'Control.Exception.throwIO', or from pure code that calls 'error'. The
-- query answers @{"status": "error", "error": <message>}@: the query server
-- responds 500 and keeps serving, and an ephemeral query exits 1.
module The0.Query
    ( handler
    , run
    , getParams
    , getConfig
    , isQueryMode
    , getParam
    , getParamAs
    , reqValues
    , Request(RequestValues, Request, reqPath, reqParams)
    , ReadOnlyStateError(..)
    , setResultPath
    ) where

import Control.DeepSeq (force)
import Control.Exception (catch, evaluate, finally, SomeException)
import Control.Monad (when)
import Data.Aeson (FromJSON, Result(..), Value(..), decodeStrict, encode, fromJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkArray(..), TkRecord(..), Tokens(..))
import Data.Aeson.Key (Key, toText)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Types (urlDecode)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import The0.Input (logError, logInfo)
import The0.Internal.Env (lookupEnvBytes)
import The0.State (ReadOnlyStateError(..))

-- | Request object passed to handlers (Express-like): the query path and
-- its params, which are JSON values.
data Request = RequestValues String (Map.Map String Value)
    deriving (Show, Eq)

-- | A request with its params as text, the way requests have always been
-- built and read: 'reqPath' is the query path, and 'reqParams' holds each
-- param as text.
pattern Request :: String -> Map.Map String String -> Request
pattern Request { reqPath, reqParams } <- (textView -> (reqPath, reqParams))
  where
    Request path params = RequestValues path (Map.map (String . T.pack) params)
{-# COMPLETE Request #-}

-- | Every param with its JSON type.
reqValues :: Request -> Map.Map String Value
reqValues (RequestValues _ values) = values

-- | A param decoded into @a@, or 'Nothing' when it is missing or does not
-- decode.
--
-- * A string param that does not decode as @a@ is parsed as JSON first, so
--   @getParamAs req "limit" :: Maybe Int@ reads @"5"@ from a URL query string
--   as 5.
-- * Asked for text, it gives any other value as its JSON text, as
--   'getParam' does, and a @null@ as 'Nothing'.
getParamAs :: FromJSON a => Request -> String -> Maybe a
getParamAs req key = Map.lookup key (reqValues req) >>= \value ->
    case (fromJSON value, value) of
        (Success typed, _) -> Just typed
        (Error _, String text) -> decodeStrict (TE.encodeUtf8 text)
        (Error _, Null) -> Nothing
        (Error _, other) -> case fromJSON (String (T.pack (valueToString other))) of
            Success typed -> Just typed
            Error _ -> Nothing

textView :: Request -> (String, Map.Map String String)
textView (RequestValues path values) = (path, Map.mapMaybe text values)
  where
    -- The runtime leaves a null param out of the query string, so text
    -- handlers have always seen it as absent.
    text Null = Nothing
    text value = Just (valueToString value)

-- | Get a parameter as text, or the default when it is missing: a string as
-- it is, any other JSON value as its JSON text. A @null@ param counts as
-- missing.
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

{-# NOINLINE resultPathRef #-}
resultPathRef :: IORef FilePath
resultPathRef = unsafePerformIO $ newIORef "/query/result.json"

-- | Test-only: write ephemeral query results to @path@ instead of
-- @/query/result.json@, which tests cannot write. The runtime always reads
-- @/query/result.json@, so bots must not call this.
setResultPath :: FilePath -> IO ()
setResultPath = writeIORef resultPathRef

-- | Register a query handler (Express-like).
--
-- @
-- Query.handler "/portfolio" $ \\req -> do
--     let symbol = Query.getParam req "symbol" "BTC/USD"
--     return $ object ["symbol" .= symbol, "positions" .= ([] :: [String])]
-- @
handler :: String -> Handler -> IO ()
handler path fn = modifyIORef' handlerRegistry (Map.insert path fn)

-- | Get current query parameters as text, as 'reqParams' gives them
-- (alternative to request object).
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
    mConfigBytes <- lookupEnvBytes "BOT_CONFIG"
    case mConfigBytes of
        Just configBytes -> case decodeStrict configBytes of
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
    resultPath <- readIORef resultPathRef
    createDirectoryIfMissing True (takeDirectory resultPath)
    BL.writeFile resultPath (encode result)
        `catch` \(e :: SomeException) ->
            hPutStrLn stderr $ "RESULT_ERROR: Failed to write result file: " ++ show e

-- | Execute single query and write result to /query/result.json.
runEphemeral :: String -> IO ()
runEphemeral queryPath = do
    mParamsBytes <- lookupEnvBytes "QUERY_PARAMS"
    let params = fromMaybe Map.empty (mParamsBytes >>= jsonObjectParams)
    outcome <- answer (RequestValues queryPath params)
    writeResult (answerBody outcome)
    when (answerStatus outcome /= 200) $ exitWith (ExitFailure 1)

-- | What a query comes to in either mode: the status the query server
-- answers with, and the body both modes send. An ephemeral query exits 0
-- only on 200.
data Answer = Answer
    { answerStatus :: Int
    , answerBody   :: Value
    }

-- | Run the handler registered for the request's path, or report that there
-- is none.
answer :: Request -> IO Answer
answer req = do
    writeIORef currentParamsRef (reqParams req)
    handlers <- readIORef handlerRegistry
    case Map.lookup (reqPath req) handlers of
        Nothing -> return $ Answer 404 $ object
            [ "status" .= ("error" :: String)
            , "error" .= ("No handler for path: " ++ reqPath req)
            , "available" .= Map.keys handlers
            ]
        Just fn -> do
            -- Forced here, so pure code that fails inside the result is a
            -- handler error rather than a failure while writing the answer.
            result <- (Right <$> (fn req >>= evaluate . force))
                `catch` \(e :: SomeException) -> return (Left (show e))
            return $ case result of
                Left err -> Answer 500 $ object
                    [ "status" .= ("error" :: String)
                    , "error" .= err
                    ]
                Right val -> Answer 200 $ object
                    [ "status" .= ("ok" :: String)
                    , "data" .= val
                    ]

-- | Start HTTP server on port 9476 for realtime bots.
runServer :: IO ()
runServer = do
    mPortStr <- lookupEnv "THE0_QUERY_PORT"
    let port = case mPortStr of
            Just ps -> case reads ps of
                [(p, "")] -> p :: Int
                _ -> 9476
            Nothing -> 9476

    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock 10

    logInfo ("Query server started on port " ++ show port) Nothing

    serverLoop sock

serverLoop :: Socket -> IO ()
serverLoop sock = do
    (clientSock, _) <- accept sock
    handleConnection clientSock `catch` logFailure
    serverLoop sock
  where
    logFailure :: SomeException -> IO ()
    logFailure e = logError ("Error handling request: " ++ show e) Nothing

handleConnection :: Socket -> IO ()
handleConnection sock = flip finally (close sock) $ do
    request <- readRequest sock
    case request of
        Just (target, body) -> do
            outcome <- answer (queryRequest target body)
            let bodyStr = BL.toStrict (encode (answerBody outcome))
                response = BS.pack (concatMap (++ "\r\n")
                    [ "HTTP/1.1 " ++ show (answerStatus outcome) ++ " " ++ reasonPhrase (answerStatus outcome)
                    , "Content-Type: application/json"
                    , "Content-Length: " ++ show (BS.length bodyStr)
                    , ""
                    ]) <> bodyStr
            sendAll sock response
        Nothing -> return ()

-- | The request target and body of one HTTP request, or 'Nothing' when the
-- connection closes before a request line with a target arrives.
readRequest :: Socket -> IO (Maybe (String, BS.ByteString))
readRequest sock = readHead BS.empty
  where
    readHead buffer = case BS.breakSubstring "\r\n\r\n" buffer of
        (headBytes, rest) | not (BS.null rest) -> do
            let headLines = map (BS.takeWhile (/= '\r')) (BS.lines headBytes)
                contentLength = fromMaybe 0 $ do
                    value <- lookup "content-length"
                        [ (map toLower (BS.unpack name), BS.unpack (BS.dropWhile (== ' ') (BS.drop 1 rest')))
                        | line <- drop 1 headLines
                        , let (name, rest') = BS.break (== ':') line
                        ]
                    case reads value of
                        [(n, rest'')] | all isSpace rest'' -> Just n
                        _ -> Nothing
            body <- readBody contentLength (BS.drop 4 rest)
            return $ case map BS.unpack . BS.words <$> listToMaybe headLines of
                Just (_method : target : _) -> Just (target, body)
                _ -> Nothing
        _ -> do
            chunk <- recv sock 4096
            if BS.null chunk then return Nothing else readHead (buffer <> chunk)

    readBody contentLength received
        | BS.length received >= contentLength = return (BS.take contentLength received)
        | otherwise = do
            chunk <- recv sock (min 65536 (contentLength - BS.length received))
            if BS.null chunk then return received else readBody contentLength (received <> chunk)

-- | A JSON object body supplies the params with their JSON types; otherwise
-- they come from the query string.
queryRequest :: String -> BS.ByteString -> Request
queryRequest target body =
    let (path, params) = parsePathAndQuery target
    in maybe (Request path params) (RequestValues path) (jsonObjectParams body)

-- | Params from JSON text holding an object, with their JSON types;
-- 'Nothing' for anything else.
--
-- An object that repeats a key, at any depth, counts as unreadable, as in
-- every SDK, rather than whichever value the decoder happens to keep.
jsonObjectParams :: BS.ByteString -> Maybe (Map.Map String Value)
jsonObjectParams bytes = case decodeStrict bytes of
    Just (Object o) | isJust (skipValue (bsToTokens bytes)) ->
        Just (Map.fromList [(T.unpack (toText k), v) | (k, v) <- KM.toList o])
    _ -> Nothing
  where
    -- Walks one value's tokens and gives what follows it, or 'Nothing' at a
    -- repeated key or an error.
    skipValue :: Tokens k e -> Maybe k
    skipValue tokens = case tokens of
        TkLit _ rest -> Just rest
        TkText _ rest -> Just rest
        TkNumber _ rest -> Just rest
        TkArrayOpen items -> skipItems items
        TkRecordOpen pairs -> skipPairs Set.empty pairs
        TkErr _ -> Nothing

    skipItems :: TkArray k e -> Maybe k
    skipItems (TkItem tokens) = skipValue tokens >>= skipItems
    skipItems (TkArrayEnd rest) = Just rest
    skipItems (TkArrayErr _) = Nothing

    skipPairs :: Set.Set Key -> TkRecord k e -> Maybe k
    skipPairs seen (TkPair key tokens)
        | key `Set.member` seen = Nothing
        | otherwise = skipValue tokens >>= skipPairs (Set.insert key seen)
    skipPairs _ (TkRecordEnd rest) = Just rest
    skipPairs _ (TkRecordErr _) = Nothing

reasonPhrase :: Int -> String
reasonPhrase 200 = "OK"
reasonPhrase 404 = "Not Found"
reasonPhrase _ = "Internal Server Error"

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

    urlDecodeString = T.unpack . TE.decodeUtf8With TE.lenientDecode . urlDecode True . BS.pack

-- | Convert a Value to a String for simple cases.
valueToString :: Value -> String
valueToString (String t) = T.unpack t
valueToString v = T.unpack (TE.decodeUtf8 (BL.toStrict (encode v)))

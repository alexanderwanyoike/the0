{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runs the contract cases: sandboxes, processes, HTTP and matching.
module Harness
    ( contractBotArg
    , spec
    , runCase
    , Sandbox
    , sandboxQueryResult
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value(..), eitherDecodeStrict, encode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit, isSpace, toLower)
import Data.List (findIndex, isSuffixOf, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, listDirectory)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (Handle, hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

contractBotArg :: String
contractBotArg = "contract-bot"

runTimeout, serverStartTimeout, stopTimeout :: Double
runTimeout = 60
serverStartTimeout = 30
stopTimeout = 10

-- sdk/contract/README.md, step 5, says why a server case may need a fresh port.
portAttempts :: Int
portAttempts = 3

-- Cases this SDK does not pass yet, each with the reason; sdk/contract/README.md
-- says how the list is kept.
knownFailures :: [(String, String)]
knownFailures =
    [ ("query-ephemeral-params", "ephemeral params turn every value into a string")
    , ("query-server-params-json-body", "the query server reads params from the query string only")
    , ("query-server-unknown-path", "the query server's 404 has no available list")
    ]

casesDir :: IO FilePath
casesDir = (</> ".." </> "contract" </> "cases") <$> getCurrentDirectory

casePaths :: IO [FilePath]
casePaths = do
    dir <- casesDir
    names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
    pure $ sort [dir </> name | name <- names, takeExtension name == ".json"]

spec :: Spec
spec = describe "bot contract" $ do
    paths <- runIO casePaths
    let names = map takeBaseName paths

    it "finds the cases" $
        unless (not (null paths)) $ do
            dir <- casesDir
            expectationFailure ("no contract cases in " ++ dir)

    it "lists only real cases in KNOWN_FAILURES" $
        [name | (name, _) <- knownFailures, name `notElem` names] `shouldBe` []

    forM_ paths $ \path ->
        it (takeBaseName path) $ checkCase path (lookup (takeBaseName path) knownFailures)

checkCase :: FilePath -> Maybe String -> Expectation
checkCase path knownFailure = do
    case_ <- either (fail . (("the case is not JSON: " ++ path ++ ": ") ++)) pure
        . eitherDecodeStrict =<< BS.readFile path
    exe <- getExecutablePath
    (problems, output) <- runCase case_ exe $ \sandbox ->
        [contractBotArg, path, sandboxQueryResult sandbox]
    case (null problems, knownFailure) of
        (True, Nothing) -> pure ()
        (False, Just _) -> pure ()
        (True, Just reason) -> expectationFailure $
            "listed in KNOWN_FAILURES (" ++ reason ++ ") but passes: take it off the list"
        (False, Nothing) -> expectationFailure (unlines problems ++ output)

-- | Runs @case_@ against the program @exe@, started with @botArgs sandbox@,
-- and returns the mismatches and the program's output.
runCase :: Value -> FilePath -> (Sandbox -> [String]) -> IO ([String], String)
runCase case_ exe botArgs = attempt 1
  where
    servesHttp = isJust (field "requests" case_)
    attempt n = do
        outcome <- withSystemTempDirectory "the0-contract-" $ \root -> do
            port <- if servesHttp then Just <$> freePort else pure Nothing
            let sandbox = Sandbox root port
            prepare case_ sandbox
            run <- runBot case_ exe (botArgs sandbox) sandbox
            if runExitedBeforeServing run && n < portAttempts
                then pure Nothing
                else do
                    problems <- compareCase case_ sandbox run
                    pure (Just (problems, runOutput run))
        maybe (attempt (n + 1)) pure outcome

-- | The directories the runtime gives a bot, under one temporary root.
data Sandbox = Sandbox
    { sandboxRoot :: FilePath
    , sandboxPort :: Maybe Int
    }

codeDir, stateDir, sandboxQueryResult :: Sandbox -> FilePath
codeDir sandbox = sandboxRoot sandbox </> "bot"
stateDir sandbox = sandboxRoot sandbox </> "state" </> ".the0-state"
sandboxQueryResult sandbox = sandboxRoot sandbox </> "query" </> "result.json"

prepare :: Value -> Sandbox -> IO ()
prepare case_ sandbox = do
    createDirectoryIfMissing True (codeDir sandbox)
    createDirectoryIfMissing True (takeDirectory (sandboxQueryResult sandbox))
    case field "state_before" case_ of
        Just (Object state) -> do
            createDirectoryIfMissing True (stateDir sandbox)
            forM_ (KM.toList state) $ \(key, value) ->
                BL.writeFile (stateDir sandbox </> K.toString key ++ ".json") (encode value)
        _ -> pure ()

sandboxEnv :: Value -> Sandbox -> [(String, String)]
sandboxEnv case_ sandbox =
    [ (K.toString key, T.unpack (foldr substitute value placeholders))
    | (key, String value) <- maybe [] KM.toList (objectField "env" case_)
    ]
  where
    substitute (placeholder, actual) = T.replace placeholder actual
    placeholders =
        [ ("{{code_mount_dir}}", T.dropWhile (== '/') (T.pack (codeDir sandbox)))
        , ("{{state_dir}}", T.pack (stateDir sandbox))
        , ("{{port}}", maybe "" (T.pack . show) (sandboxPort sandbox))
        ]

data Run = Run
    { runExitCode :: Value
    , runStdout :: BS.ByteString
    , runStderr :: BS.ByteString
    -- Mismatches found while the process ran, such as HTTP responses.
    , runProblems :: [String]
    , runExitedBeforeServing :: Bool
    }

runOutput :: Run -> String
runOutput run =
    "exit code: " ++ showJson (runExitCode run)
        ++ "\n--- stdout\n" ++ utf8 (runStdout run)
        ++ "--- stderr\n" ++ utf8 (runStderr run)

runBot :: Value -> FilePath -> [String] -> Sandbox -> IO Run
runBot case_ exe args sandbox = do
    (Just stdinHandle, Just stdoutHandle, Just stderrHandle, process) <- createProcess (proc exe args)
        { env = Just (sandboxEnv case_ sandbox)
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    hClose stdinHandle
    stdoutText <- capture stdoutHandle
    stderrText <- capture stderrHandle

    (problems, exitedBeforeServing, status) <- case sandboxPort sandbox of
        Just port -> do
            ready <- waitForPort port process
            case ready of
                Right () -> do
                    problems <- concat <$> forM (zip [0 ..] (arrayField "requests" case_))
                        (uncurry (requestMismatches port))
                    status <- stop process
                    pure (problems, False, status)
                Left notReady -> do
                    exited <- isJust <$> getProcessExitCode process
                    status <- stop process
                    pure ([notReady], exited, status)
        Nothing -> do
            status <- waitUntil process runTimeout
            case status of
                Just _ -> pure ([], False, status)
                Nothing -> do
                    killed <- kill process
                    pure (["did not exit within " ++ show (round runTimeout :: Int) ++ "s"], False, killed)

    out <- stdoutText
    err <- stderrText
    pure Run
        { runExitCode = maybe Null exitCodeValue status
        , runStdout = out
        , runStderr = err
        , runProblems = problems
        , runExitedBeforeServing = exitedBeforeServing
        }

capture :: Handle -> IO (IO BS.ByteString)
capture handle = do
    var <- newEmptyMVar
    _ <- forkIO $ do
        bytes <- try (BS.hGetContents handle)
        putMVar var (either (\(_ :: SomeException) -> BS.empty) id bytes)
    pure (takeMVar var)

waitUntil :: ProcessHandle -> Double -> IO (Maybe ExitCode)
waitUntil process seconds = do
    deadline <- (+ seconds) <$> getMonotonicTime
    let loop = do
            status <- getProcessExitCode process
            now <- getMonotonicTime
            case status of
                Just _ -> pure status
                Nothing | now >= deadline -> pure Nothing
                Nothing -> threadDelay 10000 >> loop
    loop

kill :: ProcessHandle -> IO (Maybe ExitCode)
kill process = do
    pid <- getPid process
    forM_ pid $ \p -> try (signalProcess sigKILL p) :: IO (Either IOException ())
    Just <$> waitForProcess process

-- | Stops a query server the way the runtime does, with SIGTERM.
stop :: ProcessHandle -> IO (Maybe ExitCode)
stop process = do
    exited <- getProcessExitCode process
    case exited of
        Just _ -> pure exited
        Nothing -> do
            terminateProcess process
            status <- waitUntil process stopTimeout
            maybe (kill process) (pure . Just) status

-- | A process ended by a signal has a negative exit code, as Python reports
-- it, so @nonzero@ still matches.
exitCodeValue :: ExitCode -> Value
exitCodeValue ExitSuccess = Number 0
exitCodeValue (ExitFailure code) = Number (fromIntegral code)

loopback :: PortNumber -> SockAddr
loopback port = SockAddrInet port (tupleToHostAddress (127, 0, 0, 1))

freePort :: IO Int
freePort = bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    bind sock (loopback 0)
    address <- getSocketName sock
    case address of
        SockAddrInet port _ -> pure (fromIntegral port)
        _ -> fail "cannot find a free port"

waitForPort :: Int -> ProcessHandle -> IO (Either String ())
waitForPort port process = do
    deadline <- (+ serverStartTimeout) <$> getMonotonicTime
    let loop = do
            status <- getProcessExitCode process
            case status of
                Just code -> pure $ Left $
                    "query server exited with " ++ showJson (exitCodeValue code)
                        ++ " before accepting connections"
                Nothing -> do
                    connected <- try $ bracket (socket AF_INET Stream defaultProtocol) close $ \sock ->
                        connect sock (loopback (fromIntegral port))
                    now <- getMonotonicTime
                    case connected of
                        Right () -> pure (Right ())
                        Left (_ :: IOException)
                            | now >= deadline -> pure $ Left $
                                "query server did not accept connections within "
                                    ++ show (round serverStartTimeout :: Int) ++ "s"
                            | otherwise -> threadDelay 50000 >> loop
    loop

data Response = Response
    { responseStatus :: Int
    , responseHeaders :: [(String, String)]
    , responseBody :: BS.ByteString
    }

requestMismatches :: Int -> Int -> Value -> IO [String]
requestMismatches port index request = do
    let method = textField "method" request
        target = textField "target" request
        at = "requests[" ++ show index ++ "] " ++ method ++ " " ++ target
        expected = fromMaybe Null (field "expect" request)
    response <- send port method target request
    pure $ case response of
        Left problem -> [at ++ ": " ++ problem]
        Right r ->
            mismatches (fromMaybe Null (field "status" expected))
                (Number (fromIntegral (responseStatus r))) (at ++ " status")
                ++ concat
                    [ case lookup (map toLower (K.toString name)) (responseHeaders r) of
                        Nothing -> [at ++ " header " ++ K.toString name ++ ": missing"]
                        Just got -> mismatches want (String (T.pack got))
                            (at ++ " header " ++ K.toString name)
                    | (name, want) <- maybe [] KM.toList (objectField "headers" expected)
                    ]
                ++ case eitherDecodeStrict (responseBody r) of
                    Right body -> mismatches (fromMaybe Null (field "body" expected)) body (at ++ " body")
                    Left _ -> [at ++ " body: not JSON: " ++ utf8 (responseBody r)]

-- | One request on a new connection, then the whole response.
send :: Int -> String -> String -> Value -> IO (Either String Response)
send port method target request = do
    let (body, contentType) = case (field "body" request, field "raw_body" request) of
            (Just json, _) -> (BL.toStrict (encode json), Just "application/json")
            (_, Just (String raw)) ->
                (TE.encodeUtf8 raw, Just (maybe "text/plain" T.unpack (stringField "content_type" request)))
            _ -> (BS.empty, Nothing)
        headers =
            [ "Host: 127.0.0.1:" ++ show port, "Connection: close" ]
                ++ maybe [] (\value -> ["Content-Type: " ++ value]) contentType
                ++ ["Content-Length: " ++ show (BS.length body) | not (BS.null body) || method == "POST"]
        hd = BC.pack $ concatMap (++ "\r\n") ((method ++ " " ++ target ++ " HTTP/1.1") : headers) ++ "\r\n"
    outcome <- try $ bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
        connect sock (loopback (fromIntegral port))
        sendAll sock (hd <> body)
        timeout (round (runTimeout * 1000000)) (receiveAll sock)
    pure $ case outcome of
        Left (e :: IOException) -> Left ("cannot connect or send: " ++ show e)
        Right Nothing -> Left ("no response within " ++ show (round runTimeout :: Int) ++ "s")
        Right (Just raw) -> parseResponse raw

-- | Reads until the server closes the connection. A server that closes
-- without reading the whole request may reset the connection after it has
-- sent its response, so a reset keeps what was read.
receiveAll :: Socket -> IO BS.ByteString
receiveAll sock = go []
  where
    go chunks = do
        chunk <- try (recv sock 65536)
        case chunk of
            Right bytes | not (BS.null bytes) -> go (bytes : chunks)
            Left (e :: IOException) | null chunks -> ioError e
            _ -> pure (BS.concat (reverse chunks))

parseResponse :: BS.ByteString -> Either String Response
parseResponse raw = do
    let (hd, rest) = BS.breakSubstring "\r\n\r\n" raw
    when (BS.null rest) $ Left ("not an HTTP response: " ++ utf8 raw)
    let afterHead = BS.drop 4 rest
    (statusLine, headerLines) <- case splitCrlf hd of
        line : more -> Right (line, more)
        [] -> Left ("no status line: " ++ utf8 hd)
    status <- case words statusLine of
        _ : code : _ | not (null code), all isDigit code -> Right (read code)
        _ -> Left ("no status line: " ++ utf8 hd)
    let headers =
            [ (map toLower (trim name), trim (drop 1 value))
            | line <- headerLines
            , let (name, value) = break (== ':') line
            , not (null value)
            ]
        body = case lookup "content-length" headers of
            Just n | all isDigit n, not (null n), read n <= BS.length afterHead -> BS.take (read n) afterHead
            _ -> afterHead
    Right (Response status headers body)
  where
    splitCrlf bytes
        | BS.null bytes = []
        | otherwise =
            let (line, rest) = BS.breakSubstring "\r\n" bytes
            in utf8 line : if BS.null rest then [] else splitCrlf (BS.drop 2 rest)
    trim = dropWhileEnd' isSpace . dropWhile isSpace
    dropWhileEnd' p = reverse . dropWhile p . reverse

compareCase :: Value -> Sandbox -> Run -> IO [String]
compareCase case_ sandbox run = case objectField "expect" case_ of
    Nothing -> pure (runProblems run)
    Just expected -> do
        let want name = KM.lookup (K.fromString name) expected
        result <- maybe (pure []) (\w -> fileMismatches w (codeDir sandbox </> "result.json") "result") (want "result")
        queryResult <- maybe (pure []) (\w -> fileMismatches w (sandboxQueryResult sandbox) "query_result") (want "query_result")
        stateAfter <- case want "state_after" of
            Nothing -> pure []
            Just w -> (\state -> mismatches w state "state_after") <$> readState (stateDir sandbox)
        pure $ runProblems run
            ++ maybe [] (\w -> mismatches w (runExitCode run) "exit_code") (want "exit_code")
            ++ maybe [] (\w -> lineMismatches w (runStdout run) "stdout") (want "stdout")
            ++ maybe [] (\w -> lineMismatches w (runStderr run) "stderr") (want "stderr")
            ++ result ++ queryResult ++ stateAfter

lineMismatches :: Value -> BS.ByteString -> String -> [String]
lineMismatches expected bytes stream
    | length lns /= length wanted =
        [stream ++ ": expected " ++ show (length wanted) ++ " lines, got " ++ show (length lns)]
    | otherwise = concat
        [ case eitherDecodeStrict (TE.encodeUtf8 (T.pack line)) of
            Right got -> mismatches want got (stream ++ "[" ++ show index ++ "]")
            Left _ -> [stream ++ "[" ++ show index ++ "]: not JSON: " ++ line]
        | (index, want, line) <- zip3 [0 :: Int ..] wanted lns
        ]
  where
    wanted = case expected of
        Array items -> V.toList items
        _ -> []
    lns = filter (not . all isSpace) (lines (utf8 bytes))

fileMismatches :: Value -> FilePath -> String -> IO [String]
fileMismatches Null path name = do
    exists <- doesFileExist path
    pure [name ++ ": expected no file, found one" | exists]
fileMismatches expected path name = do
    bytes <- try (BS.readFile path)
    pure $ case bytes of
        Left (_ :: IOException) -> [name ++ ": no file written"]
        Right content -> case eitherDecodeStrict content of
            Right actual -> mismatches expected actual name
            Left e -> [name ++ ": not JSON: " ++ e]

readState :: FilePath -> IO Value
readState dir = do
    names <- either (\(_ :: IOException) -> []) id <$> try (listDirectory dir)
    entries <- forM [name | name <- names, takeExtension name == ".json"] $ \name -> do
        content <- BS.readFile (dir </> name)
        let value = either (const (String (TE.decodeUtf8With TE.lenientDecode content))) id
                (eitherDecodeStrict content)
        _ <- evaluate value
        pure (K.fromString (takeBaseName name), value)
    pure (Object (KM.fromList entries))

-- | Every way @actual@ fails to match @expected@, as readable lines.
mismatches :: Value -> Value -> String -> [String]
mismatches expected actual at = case expected of
    Object want | Just (String kind) <- KM.lookup "$match" want ->
        case matches kind want actual of
            Right True -> []
            Right False -> differ
            Left unknown -> [at ++ ": " ++ unknown]
    Object want -> case actual of
        Object got ->
            concat
                [ case KM.lookup key got of
                    Nothing -> [at ++ "." ++ K.toString key ++ ": missing"]
                    Just gotValue -> mismatches wantValue gotValue (at ++ "." ++ K.toString key)
                | (key, wantValue) <- KM.toList want
                ]
                ++ [ at ++ "." ++ K.toString key ++ ": unexpected, got " ++ showJson gotValue
                   | (key, gotValue) <- KM.toList got
                   , not (KM.member key want)
                   ]
        _ -> [at ++ ": expected an object, got " ++ showJson actual]
    Array want -> case actual of
        Array got | V.length got == V.length want ->
            concat
                [ mismatches w g (at ++ "[" ++ show index ++ "]")
                | (index, w, g) <- zip3 [0 :: Int ..] (V.toList want) (V.toList got)
                ]
        _ -> differ
    -- Scientific compares by value, so 5 matches 5.0.
    _ | expected == actual -> []
    _ -> differ
  where
    differ = [at ++ ": expected " ++ showJson expected ++ ", got " ++ showJson actual]

-- | Whether @actual@ matches a @{"$match": ...}@ matcher.
matches :: T.Text -> KM.KeyMap Value -> Value -> Either String Bool
matches kind matcher actual = case kind of
    "timestamp" -> Right $ case actual of
        String text -> isTimestamp (T.unpack text)
        _ -> False
    "string" -> Right $ case actual of
        String _ -> True
        _ -> False
    "contains" -> Right $ case (KM.lookup "value" matcher, actual) of
        (Just (String part), String text) -> part `T.isInfixOf` text
        _ -> False
    "nonzero" -> Right $ case actual of
        Number n -> n /= 0
        _ -> False
    "unordered" -> Right $ case (KM.lookup "items" matcher, actual) of
        (Just (Array items), Array got) -> unorderedMatches (V.toList items) (V.toList got)
        _ -> False
    _ -> Left ("unknown matcher " ++ show kind)

unorderedMatches :: [Value] -> [Value] -> Bool
unorderedMatches items actual
    | length items /= length actual = False
    | otherwise = go items actual
  where
    go [] _ = True
    go (item : rest) remaining =
        case findIndex (\got -> null (mismatches item got "")) remaining of
            Nothing -> False
            Just index -> go rest (take index remaining ++ drop (index + 1) remaining)

-- | @^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$@
isTimestamp :: String -> Bool
isTimestamp text =
    length text > length shape
        && and (zipWith fits shape text)
        && fraction (drop (length shape) text)
  where
    shape = "dddd-dd-ddTdd:dd:dd" :: String
    fits 'd' c = isDigit c
    fits want c = want == c
    fraction "Z" = True
    fraction ('.' : rest) =
        "Z" `isSuffixOf` rest && length rest > 1 && all isDigit (init rest)
    fraction _ = False

field :: T.Text -> Value -> Maybe Value
field name (Object o) = KM.lookup (K.fromText name) o
field _ _ = Nothing

objectField :: T.Text -> Value -> Maybe (KM.KeyMap Value)
objectField name value = case field name value of
    Just (Object o) -> Just o
    _ -> Nothing

stringField :: T.Text -> Value -> Maybe T.Text
stringField name value = case field name value of
    Just (String text) -> Just text
    _ -> Nothing

textField :: T.Text -> Value -> String
textField name = maybe "" T.unpack . stringField name

arrayField :: T.Text -> Value -> [Value]
arrayField name value = case field name value of
    Just (Array items) -> V.toList items
    _ -> []

showJson :: Value -> String
showJson = utf8 . BL.toStrict . encode

utf8 :: BS.ByteString -> String
utf8 = T.unpack . TE.decodeUtf8With TE.lenientDecode

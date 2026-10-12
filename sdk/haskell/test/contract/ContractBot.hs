{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The contract bot: plays one sdk/contract case through the SDK's public
-- API, the way a bot would. Each op makes the SDK call sdk/contract/README.md
-- maps it to. The harness starts it with the case's environment.
module ContractBot (main) where

import Control.Exception (SomeException, catch, fromException, throwIO)
import Control.Monad (forM_, void)
import Data.Aeson (Value(..), eitherDecodeFileStrict, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (ExitCode, die)
import qualified The0.Input as Input
import qualified The0.Query as Query
import qualified The0.State as State

type Bound = Map.Map T.Text Value

main :: FilePath -> FilePath -> IO ()
main casePath queryResultPath = do
    case_ <- either (die . ("contract bot: the case is not JSON: " ++)) pure
        =<< eitherDecodeFileStrict casePath
    Query.setResultPath queryResultPath

    case field "entrypoint" case_ of
        Just "bot" -> void (runSteps (array (field "script" case_)) Nothing)
        _ -> do
            let handlers = case field "handlers" case_ of
                    Just (Object o) -> KM.toList o
                    _ -> []
            forM_ handlers $ \(path, steps) ->
                Query.handler (K.toString path) $ \req -> runSteps (array (Just steps)) (Just req)
            Query.run

resolve :: Bound -> Value -> Value
resolve bound value = case value of
    Object o
        | [("$ref", String name)] <- KM.toList o ->
            fromMaybe (Prelude.error ("contract bot: nothing is bound to " ++ show name))
                (Map.lookup name bound)
        | otherwise -> Object (KM.map (resolve bound) o)
    Array items -> Array (V.map (resolve bound) items)
    other -> other

runSteps :: [Value] -> Maybe Query.Request -> IO Value
runSteps steps request = go Map.empty steps
  where
    go _ [] = pure Null
    go bound (step : rest)
        | field "op" step == Just "return" = pure (resolve bound (fromMaybe Null (field "value" step)))
        | otherwise = do
            value <- catching (field "catch" step) (perform step request bound)
            let bound' = case field "as" step of
                    Just (String name) -> Map.insert name value bound
                    _ -> bound
            go bound' rest

catching :: Maybe Value -> IO Value -> IO Value
catching kind action = case kind of
    Nothing -> action
    Just "any" -> action `catch` \(e :: SomeException) ->
        case fromException e of
            Just (exit :: ExitCode) -> throwIO exit
            Nothing -> pure (caught "any")
    -- The query module's type, which must be the one state fails with.
    Just "read_only_state" -> action `catch` \(_ :: Query.ReadOnlyStateError) ->
        pure (caught "read_only_state")
    Just other -> Prelude.error ("contract bot: unknown catch kind " ++ show other)
  where
    caught name = object ["caught" .= (name :: T.Text)]

perform :: Value -> Maybe Query.Request -> Bound -> IO Value
perform step request bound = case op of
    "parse" -> do
        (botId, config) <- Input.parse
        pure (object ["bot_id" .= botId, "config" .= config])
    "success" -> Null <$ case arg "data" of
        Just payload -> Input.successWithData (text "message") payload
        Nothing -> Input.success (text "message")
    "error" -> case arg "data" of
        Just payload -> Input.errorWithData (text "message") payload
        Nothing -> Input.error (text "message")
    "result" -> Null <$ Input.result (value "value")
    "metric" -> Null <$ Input.metric (text "type") (value "data")
    "log" -> Null <$ Input.log (text "message") (arg "data") (logLevel <$> arg "level")
    "raise" -> ioError (userError (text "message"))

    "state.get" -> fromMaybe (value "default") <$> State.get (text "key")
    "state.set" -> Null <$ State.set (text "key") (value "value")
    "state.delete" -> Bool <$> State.delete (text "key")
    "state.list" -> toJSON <$> State.list
    "state.exists" -> Bool <$> State.exists (text "key")
    "state.clear" -> Null <$ State.clear

    "query.params" -> pure (toJSON (Query.reqParams req))
    "query.param" -> pure $
        maybe (value "default") toJSON (Map.lookup (text "key") (Query.reqParams req))
    "query.config" -> Query.getConfig

    _ -> Prelude.error ("contract bot: unknown op " ++ show op)
  where
    op = fromMaybe Null (field "op" step)
    args = case step of
        Object o -> KM.filterWithKey (\key _ -> key `notElem` ["op", "as", "catch"]) o
        _ -> KM.empty
    arg name = resolve bound <$> KM.lookup name args
    value = fromMaybe Null . arg
    text name = case arg name of
        Just (String t) -> T.unpack t
        _ -> Prelude.error ("contract bot: op " ++ show op ++ " needs a string " ++ show name)
    req = fromMaybe (Prelude.error ("contract bot: op " ++ show op ++ " is only valid inside a query handler")) request

logLevel :: Value -> Input.LogLevel
logLevel level = case level of
    "info" -> Input.Info
    "warn" -> Input.Warn
    "error" -> Input.Error
    _ -> Prelude.error ("contract bot: unknown log level " ++ show level)

field :: T.Text -> Value -> Maybe Value
field name (Object o) = KM.lookup (K.fromText name) o
field _ _ = Nothing

array :: Maybe Value -> [Value]
array (Just (Array items)) = V.toList items
array _ = []

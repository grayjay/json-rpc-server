{-# LANGUAGE MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
, Rank2Types
, OverloadedStrings #-}

module Data.JsonRpc.Server ( RpcResult
                           , RpcError
                           , Param(..)
                           , MethodParams
                           , JsonFunction
                           , toJsonFunction
                           , JsonFunctions
                           , toJsonFunctions
                           , call
                           , callWithBatchStrategy
                           , liftToResult
                           , rpcError
                           , rpcErrorWithData) where

import Data.String
import Data.Text (Text, append, unpack)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (toList)
import qualified Data.HashMap.Strict as H
import Data.Attoparsec.Number (Number)
import Control.Applicative ((<$>), (<*>), empty)
import Control.Monad (liftM, mzero)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Error (Error, ErrorT, lift, runErrorT, throwError, strMsg, noMsg)
import Prelude hiding (length)

data Param a = Param Text (Maybe a)

type RpcResult m r = ErrorT RpcError m r

class Monad m => MethodParams f p m r | f -> p m r where
    mpApply :: f -> p -> H.HashMap Text Value -> RpcResult m r

instance (ToJSON r, Monad m) => MethodParams (RpcResult m r) () m r where
    mpApply r _ _ = r

instance (FromJSON a, ToJSON r, MethodParams f p m r) => MethodParams (a -> f) (Param a, p)m r where
    mpApply = applyFunction

applyFunction :: (FromJSON a, MethodParams f p m r)
              => (a -> f)
              -> (Param a, p)
              -> H.HashMap Text Value
              -> RpcResult m r
applyFunction f ((Param name d), ps) args = do
        maybeArg' <- maybeArg
        case maybeArg' of
          Nothing -> throwError $ RpcError (-32602) ("Cannot find required argument: " `append` name) Nothing
          Just arg -> mpApply (f arg) ps args
    where maybeArg = case H.lookup name args of
                       Nothing -> return $ d
                       Just val -> case fromJSON val of
                                     Error msg -> throwError $ rpcErrorWithData (-32602) ("Wrong type for argument: " `append` name) (Just msg)
                                     Success x -> return $ Just x

data RpcError = RpcError { errCode :: Int
                         , errMsg :: Text
                         , errData :: (Maybe Value)}
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = [codeKey .= toJSON code, msgKey .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> [dataKey .= toJSON d]) data'

data Response = Response { rspId :: Id
                         , rspResult :: Either RpcError Value }

instance ToJSON Response where
    toJSON r = object ["jsonrpc" .= jsonRpcVersion, result, "id" .= (toJSON $ rspId r)]
        where result = either (("error" .=) . toJSON) ("result" .=) (rspResult r)

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

data Id = IdString Text | IdNumber Number | IdNull

instance FromJSON Id where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull
    parseJSON _ = empty

instance ToJSON Id where
    toJSON i = case i of
                 IdString x -> toJSON x
                 IdNumber x -> toJSON x
                 IdNull -> Null

data Request = Request { rqName :: Text
                       , rqParams :: Either Object Array
                       , rqId :: Maybe Id }

instance FromJSON Request where
    parseJSON (Object x) = Request <$>
                           x .: methodKey <*>
                           (parseParams =<< x .:? paramsKey .!= emptyObject) <*>
                           x .:? idKey
        where parseParams :: Value -> Parser (Either Object Array)
              parseParams = withObject (unpack paramsKey) (return . Left)
    parseJSON _ = mzero

instance ToJSON Request where
    toJSON (Request name (Left params) i) = object pairs
        where pairs = [ methodKey .= toJSON name
                      , paramsKey .= toJSON params
                      , idKey .= toJSON i ]

rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

codeKey :: Text
codeKey = "code"

msgKey :: Text
msgKey = "message"

dataKey :: Text
dataKey = "data"

methodKey :: Text
methodKey = "method"

paramsKey :: Text
paramsKey = "params"

idKey :: Text
idKey = "id"

data JsonFunction m = JsonFunction Text (H.HashMap Text Value -> RpcResult m Value)

newtype JsonFunctions m = JsonFunctions (H.HashMap Text (JsonFunction m))

toJsonFunction :: (MethodParams f p m r, ToJSON r, Monad m) => Text -> f -> p -> JsonFunction m
toJsonFunction name f params = JsonFunction name g
    where g x = toJSON `liftM` mpApply f params x

toJsonFunctions :: [JsonFunction m] -> JsonFunctions m
toJsonFunctions fs = JsonFunctions $ H.fromList $ map (\f@(JsonFunction n _) -> (n, f)) fs

call :: Monad m => JsonFunctions m -> B.ByteString -> m (Maybe B.ByteString)
call = callWithBatchStrategy sequence

callWithBatchStrategy :: Monad m => (forall a . [m a] -> m [a]) -> JsonFunctions m -> B.ByteString -> m (Maybe B.ByteString)
callWithBatchStrategy strategy fs input = response2 response
    where response = runIdentity $ runErrorT $ do
                       val <- parseJson input
                       case val of
                                obj@(Object _) -> return $ ((toJSON <$>) `liftM` singleCall fs obj)
                                (Array vector) -> return $ ((toJSON <$>) `liftM` batchCall strategy fs (toList vector))
                                _ -> throwError $ invalidJsonRpc (Just ("Not a JSON object or array" :: String))
          response2 r = case r of
                          Left err -> return $ Just $ encode $ toJSON $ toResponse (Just IdNull) ((Left err) :: Either RpcError ())
                          Right maybeVal -> (encode <$>) `liftM` maybeVal
          parseJson = maybe invalidJson return . decode
          invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => JsonFunctions m -> Value -> m (Maybe Response)
singleCall (JsonFunctions fs) val = case fromJSON val of
                                      Error msg -> return $ toResponse (Just IdNull) $ ((Left $ invalidJsonRpc $ Just msg) :: Either RpcError ())
                                      Success (Request name (Left params) i) -> (toResponse i `liftM`) $ runErrorT $ do
                                                                                   JsonFunction _ f <- lookupMethod name
                                                                                   f params
    where lookupMethod name = maybe (methodNotFound name) return $ (H.lookup name fs)
          methodNotFound name = throwError $ rpcError (-32601) ("Method not found: " `append` name)

invalidJsonRpc :: Maybe String -> RpcError
invalidJsonRpc = rpcErrorWithData (-32600) "Invalid JSON RPC 2.0 request"

batchCall :: Monad m => (forall a. [m a] -> m [a]) -> JsonFunctions m -> [Value] -> m (Maybe [Response])
batchCall f gs vals = filterJust `liftM` results
    where results = f $ map (singleCall gs) vals
          filterJust rs = case catMaybes rs of
                            [] -> Nothing
                            xs -> Just xs

toResponse :: ToJSON a => Maybe Id -> Either RpcError a -> Maybe Response
toResponse Nothing _ = Nothing
toResponse (Just i) r = Just $ Response i (either Left (Right . toJSON) r)

liftToResult :: Monad m => m a -> RpcResult m a
liftToResult = lift

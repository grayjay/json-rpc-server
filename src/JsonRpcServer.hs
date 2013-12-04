{-# LANGUAGE MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
, Rank2Types
, OverloadedStrings #-}

module JsonRpcServer ( RpcResult
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

import Data.Text hiding (map)
import Data.Maybe (catMaybes)
import Data.String
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (toList)
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Error
import Prelude hiding (length)

data RpcError = RpcError Int Text (Maybe Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = ["code" .= toJSON code, "message" .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> ["data" .= toJSON d]) data'

type RpcResult m a = ErrorT RpcError m a

data Param a = Param Text (Maybe a)
             deriving Show

class Monad m => MethodParams m a p | a -> m p where
    mpApply :: a -> p -> H.HashMap Text Value -> RpcResult m Value

instance (ToJSON a, Monad m) => MethodParams m (RpcResult m a) () where
    mpApply r _ _ = liftM toJSON r

instance (FromJSON a, MethodParams m b p) => MethodParams m (a -> b) (Param a, p) where
    mpApply = applyFunction

applyFunction :: (FromJSON a, MethodParams m b p) => (a -> b)
              -> (Param a, p)
              -> H.HashMap Text Value
              -> RpcResult m Value
applyFunction f ((Param pName d), ps) args = let arg = maybe d parseArg $ H.lookup pName args
                                                 parseArg x = case fromJSON x of
                                                                 Error _ -> Nothing
                                                                 Success val -> Just val
                                             in case arg of
                                                  Nothing -> throwError $ RpcError (-32602) "Cannot find required argument" Nothing
                                                  Just arg' -> mpApply (f arg') ps args

data Request = Request { rqName :: Text
                       , rqParams :: Either Object Array
                       , rqId :: Maybe Id }

instance FromJSON Request where
    parseJSON (Object x) = Request <$>
                           x .: "method" <*>
                           (parseParams =<< x .:? "params" .!= emptyObject) <*>
                           x .:? "id"
        where parseParams :: Value -> Parser (Either Object Array)
              parseParams = withObject "params" (return . Left)
    parseJSON _ = mzero

data Id = IdString Text | IdNumber Number | IdNull

instance FromJSON Id where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull
    parseJSON _ = mzero

instance ToJSON Id where
    toJSON i = case i of
                 IdString x -> toJSON x
                 IdNumber x -> toJSON x
                 IdNull -> Null

data Response = Response { rspResult :: Either RpcError Value
                         , rspId :: Id }

instance ToJSON Response where
    toJSON r = object ["jsonrpc" .= jsonRpcVersion, result, "id" .= (toJSON $ rspId r)]
        where result = either (("error" .=) . toJSON) ("result" .=) (rspResult r)

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

data JsonFunction m = JsonFunction Text (H.HashMap Text Value -> RpcResult m Value)

newtype JsonFunctions m = JsonFunctions (H.HashMap Text (JsonFunction m))

toJsonFunction :: MethodParams m a p => Text -> a -> p -> JsonFunction m
toJsonFunction name f params = JsonFunction name $ mpApply f params

toJsonFunctions :: [JsonFunction m] -> JsonFunctions m
toJsonFunctions fs = JsonFunctions $ H.fromList $ map (\f@(JsonFunction n _) -> (n, f)) fs

call :: Monad m => JsonFunctions m -> B.ByteString -> m (Maybe B.ByteString)
call = callWithBatchStrategy sequence

callWithBatchStrategy :: Monad m => (forall a . [m a] -> m [a]) -> JsonFunctions m -> B.ByteString -> m (Maybe B.ByteString)
callWithBatchStrategy strategy fs input = response2 response
    where response = runIdentity $ runErrorT $ do
                       val <- parseJson input
                       case val of
                                obj@(Object _) -> return $ ((toJSON <$>)`liftM` singleCall fs obj)
                                (Array vector) -> return $ ((toJSON <$>) `liftM` batchCall strategy fs (toList vector))
                                _ -> throwError $ invalidJsonRpc (Just ("Not a JSON object or array" :: String))
          response2 r = case r of
                          Left err -> return $ Just $ encode $ toJSON $ toResponse (Just IdNull) (Left err)
                          Right maybeVal -> (encode <$>) `liftM` maybeVal
          parseJson = maybe invalidJson return . decode
          invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => JsonFunctions m -> Value -> m (Maybe Response)
singleCall (JsonFunctions fs) val = case fromJSON val of
                                      Error msg -> return $ toResponse (Just IdNull) $ Left $ invalidJsonRpc $ Just msg
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

toResponse :: Maybe Id -> Either RpcError Value -> Maybe Response
toResponse Nothing _ = Nothing
toResponse (Just i) r = Just $ Response r i

liftToResult :: Monad m => m a -> RpcResult m a
liftToResult = lift

rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

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

import Data.JsonRpc.Common
import Data.Text hiding (map)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Vector (toList)
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Error (ErrorT, lift, runErrorT, throwError)
import Prelude hiding (length)

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

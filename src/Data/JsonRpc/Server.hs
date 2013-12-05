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
import Data.Aeson.Types (Parser, emptyObject)
import Data.Vector (toList)
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Error (ErrorT, lift, runErrorT, throwError)
import Prelude hiding (length)

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
                                obj@(Object _) -> return $ ((toJSON <$>) `liftM` singleCall fs obj)
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
toResponse (Just i) r = Just $ Response i r

liftToResult :: Monad m => m a -> RpcResult m a
liftToResult = lift

rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

{-# LANGUAGE MultiParamTypeClasses,
             Rank2Types,
             TypeOperators,
             OverloadedStrings #-}

-- | Functions for implementing the server side of JSON RPC 2.0.
--   Here is an example of a simple Happstack server with three methods:
--   
--   
module Data.JsonRpc.Server ( RpcResult
                           , RpcError
                           , Parameter(..)
                           , (:+:) (..)
                           , MethodParams
                           , Method
                           , toMethod
                           , Methods
                           , toMethods
                           , call
                           , callWithBatchStrategy
                           , rpcError
                           , rpcErrorWithData) where

import Data.JsonRpc.Types
import Data.String
import Data.Text (Text, append)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Prelude hiding (length)

apply :: MethodParams f p m r => f -> p -> Args -> RpcResult m r
apply f p (Left hm) = mpApplyNamed f p hm
apply f p (Right vec) = mpApplyUnnamed f p vec

-- | Creates a method from a name, function, and parameter description.
toMethod :: (MethodParams f p m r, ToJSON r, Monad m) => Text -> f -> p -> Method m
toMethod name f params = Method name (\args -> toJSON `liftM` apply f params args)

-- | Creates a set of methods to be called by name. The names must be unique.
toMethods :: [Method m] -> Methods m
toMethods fs = Methods $ H.fromList $ map (\f@(Method n _) -> (n, f)) fs

-- | Handles one JSON RPC request. It is the same as
--   @callWithBatchStrategy sequence@.
call :: Monad m => Methods m -- ^ Choice of methods to call.
     -> B.ByteString               -- ^ JSON RPC request.
     -> m (Maybe B.ByteString)     -- ^ The response wrapped in 'Just', or
                                   --   'Nothing' in the case of a notification,
                                   --   all wrapped in the given monad.
call = callWithBatchStrategy sequence

-- | Handles one JSON RPC request.
callWithBatchStrategy :: Monad m =>
                         (forall a . [m a] -> m [a]) -- ^ Function specifying the
                                                     --   evaluation strategy.
                      -> Methods m                   -- ^ Choice of methods to call.
                      -> B.ByteString                -- ^ JSON RPC request.
                      -> m (Maybe B.ByteString)      -- ^ The response wrapped in 'Just', or
                                                     --   'Nothing' in the case of a notification,
                                                     --   all wrapped in the given monad.
callWithBatchStrategy strategy fs input = response2 response
    where response = runIdentity $ runErrorT $ do
                       val <- parseJson input
                       case val of
                         obj@(Object _) -> return ((toJSON <$>) `liftM` singleCall fs obj)
                         Array vector | V.null vector -> throwError $ rpcError (-32600) "empty batch request"
                                      | otherwise -> return ((toJSON <$>) `liftM` batchCall strategy fs (V.toList vector))
                         _ -> throwError $ invalidJsonRpc (Just ("Not a JSON object or array" :: String))
          response2 r = case r of
                          Left err -> return $ Just $ encode $ toJSON $ toResponse (Just IdNull) (Left err :: Either RpcError ())
                          Right maybeVal -> (encode <$>) `liftM` maybeVal
          parseJson = maybe invalidJson return . decode
          invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => Methods m -> Value -> m (Maybe Response)
singleCall (Methods fs) val = case fromJSON val of
                                Error msg -> return $ toResponse (Just IdNull) ((Left $ invalidJsonRpc $ Just msg) :: Either RpcError ())
                                Success (Request name params i) -> (toResponse i `liftM`) $ runErrorT $ do
                                                                     Method _ f <- lookupMethod name
                                                                     f params
    where lookupMethod name = maybe (methodNotFound name) return $ H.lookup name fs
          methodNotFound name = throwError $ rpcError (-32601) ("Method not found: " `append` name)

invalidJsonRpc :: Maybe String -> RpcError
invalidJsonRpc = rpcErrorWithData (-32600) "Invalid JSON RPC 2.0 request"

batchCall :: Monad m => (forall a. [m a] -> m [a]) -> Methods m -> [Value] -> m (Maybe [Response])
batchCall f gs vals = filterJust `liftM` results
    where results = f $ map (singleCall gs) vals
          filterJust rs = case catMaybes rs of
                            [] -> Nothing
                            xs -> Just xs

toResponse :: ToJSON a => Maybe Id -> Either RpcError a -> Maybe Response
toResponse Nothing _ = Nothing
toResponse (Just i) r = Just $ Response i (either Left (Right . toJSON) r)

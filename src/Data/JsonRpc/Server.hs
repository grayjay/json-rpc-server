{-# LANGUAGE MultiParamTypeClasses,
             Rank2Types,
             TypeOperators,
             OverloadedStrings #-}

-- | Functions for implementing the server side of JSON RPC 2.0.
--   See <http://www.jsonrpc.org/specification>.
module Data.JsonRpc.Server (
                          -- ** Instructions
                          -- $instructions

                          -- ** Unnamed and Optional Arguments
                          -- $arguments

                          -- ** Example
                          -- $example

                          -- ** Methods
                             RpcResult
                           , Method
                           , toMethod
                           , Methods
                           , toMethods
                           , call
                           , callWithBatchStrategy
                           , Parameter(..)
                           , (:+:) (..)
                           , MethodParams
                          -- ** Errors
                           , RpcError
                           , rpcError
                           , rpcErrorWithData) where

import Data.JsonRpc.Types
import Data.Text (Text, append, pack)
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

-- $instructions
-- * Create methods by calling 'toMethod' and providing the method
--   names, lists of parameters, and functions to be called.
--
-- * Create a set of methods by calling 'toMethods'.
--
-- * Process a request by calling 'call' or 'callWithBatchStrategy'
--   on the 'Methods' and input 'B.ByteString'.

-- $arguments
-- RPC methods can have any mix of required and optional parameters.
-- When a request uses unnamed arguments, the function is applied to
-- the arguments in order.  The function will be called as long as
-- all required arguments are specified, and the number of arguments
-- provided is not greater than the total number of required and
-- optional parameters.

-- $example
-- Here is an example of a simple Happstack server with three methods.
-- Compile it with the build flag @include-demo@.
--   
--   

-- | Creates a method from a name, function, and parameter descriptions.
toMethod :: (MethodParams f p m r, ToJSON r, Monad m) => Text -> f -> p -> Method m
toMethod name f params = let f' args = toJSON <$> apply f params args
                         in Method name f'

-- | Creates a set of methods to be called by name. The names must be unique.
toMethods :: [Method m] -> Methods m
toMethods fs = Methods $ H.fromList $ map pair fs
    where pair mth@(Method name _) = (name, mth)

-- | Handles one JSON RPC request. It is the same as
--   @callWithBatchStrategy sequence@.
call :: Monad m => Methods m   -- ^ Choice of methods to call.
     -> B.ByteString           -- ^ JSON RPC request.
     -> m (Maybe B.ByteString) -- ^ The response wrapped in 'Just', or
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
                         _ -> throwError $ invalidJsonRpc (Just "Not a JSON object or array")
          response2 r = case r of
                          Left err -> return $ Just $ encode $ toJSON $ toResponse (Just IdNull) (Left err :: Either RpcError ())
                          Right maybeVal -> (encode <$>) `liftM` maybeVal
          parseJson = maybe invalidJson return . decode
          invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => Methods m -> Value -> m (Maybe Response)
singleCall (Methods fs) val = case fromJSON val of
                                Error msg -> return $ toResponse (Just IdNull) ((Left $ invalidJsonRpc $ Just $ pack msg) :: Either RpcError ())
                                Success (Request name params i) -> (toResponse i `liftM`) $ runErrorT $ do
                                                                     Method _ f <- lookupMethod name
                                                                     f params
    where lookupMethod name = maybe (methodNotFound name) return $ H.lookup name fs
          methodNotFound name = throwError $ rpcError (-32601) ("Method not found: " `append` name)

invalidJsonRpc :: Maybe Text -> RpcError
invalidJsonRpc = rpcErrorWithData (-32600) "Invalid JSON RPC 2.0 request"

batchCall :: Monad m => (forall a. [m a] -> m [a])
          -> Methods m
          -> [Value]
          -> m (Maybe [Response])
batchCall strategy mths vals = (noNull . catMaybes) `liftM` results
    where results = strategy $ map (singleCall mths) vals
          noNull rs = if null rs then Nothing else Just rs

toResponse :: ToJSON a => Maybe Id -> Either RpcError a -> Maybe Response
toResponse (Just i) r = Just $ Response i $ toJSON <$> r
toResponse Nothing _ = Nothing

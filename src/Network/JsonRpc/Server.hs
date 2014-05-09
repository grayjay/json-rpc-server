{-# LANGUAGE MultiParamTypeClasses,
             Rank2Types,
             TypeOperators,
             OverloadedStrings #-}

-- | Functions for implementing the server side of JSON RPC 2.0.
--   See <http://www.jsonrpc.org/specification>.
module Network.JsonRpc.Server (
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

import Network.JsonRpc.Types
import Data.Text (Text, append, pack)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>))
import Control.Monad (liftM)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Error (ErrorT, runErrorT, throwError)

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
-- Compile it with the build flag @demo@.
--   
-- > <insert Demo.hs>
--   

-- | Creates a method from a name, function, and parameter descriptions.
--   The parameter names must be unique.
toMethod :: (MethodParams f p m r, A.ToJSON r, Monad m) => Text -> f -> p -> Method m
toMethod name f params = let f' args = A.toJSON <$> apply f params args
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
callWithBatchStrategy strategy fs input = either returnErr callMethod request
    where request :: Either RpcError (Either A.Value [A.Value])
          request = runIdentity $ runErrorT $ parseVal =<< parseJson input
          parseJson = maybe invalidJson return . A.decode
          parseVal val = case val of
                           obj@(A.Object _) -> return $ Left obj
                           A.Array vec | V.null vec -> throwInvalidRpc "Empty batch request"
                                       | otherwise -> return $ Right $ V.toList vec
                           _ -> throwInvalidRpc "Not a JSON object or array"
          callMethod rq = case rq of
                            Left val -> encodeJust `liftM` singleCall fs val
                            Right vals -> encodeJust `liftM` batchCall strategy fs vals
              where encodeJust r = A.encode <$> r
          returnErr = return . Just . A.encode . nullIdResponse
          invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => Methods m -> A.Value -> m (Maybe Response)
singleCall (Methods fs) val = case parsed of
                                Left err -> return $ nullIdResponse err
                                Right (Request name args i) ->
                                  toResponse i `liftM` runErrorT (applyMethodTo args =<< method)
                                    where method = lookupMethod name fs
    where parsed = runIdentity $ runErrorT $ parseValue val
          applyMethodTo args (Method _ f) = f args

nullIdResponse :: RpcError -> Maybe Response
nullIdResponse err = toResponse (Just IdNull) (Left err :: Either RpcError ())

parseValue :: (A.FromJSON a, Monad m) => A.Value -> RpcResult m a
parseValue val = case A.fromJSON val of
                   A.Error msg -> throwInvalidRpc $ pack msg
                   A.Success x -> return x

lookupMethod :: Monad m => Text -> H.HashMap Text (Method m) -> RpcResult m (Method m)
lookupMethod name = maybe notFound return . H.lookup name
    where notFound = throwError $ rpcError (-32601) $ "Method not found: " `append` name

throwInvalidRpc :: Monad m => Text -> RpcResult m a
throwInvalidRpc = throwError . rpcErrorWithData (-32600) "Invalid JSON RPC 2.0 request"

batchCall :: Monad m => (forall a. [m a] -> m [a])
          -> Methods m
          -> [A.Value]
          -> m (Maybe [Response])
batchCall strategy mths vals = (noNull . catMaybes) `liftM` results
    where results = strategy $ map (singleCall mths) vals
          noNull rs = if null rs then Nothing else Just rs

toResponse :: A.ToJSON a => Maybe Id -> Either RpcError a -> Maybe Response
toResponse (Just i) r = Just $ Response i $ A.toJSON <$> r
toResponse Nothing _ = Nothing

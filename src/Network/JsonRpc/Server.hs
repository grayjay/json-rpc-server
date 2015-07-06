{-# LANGUAGE CPP,
             MultiParamTypeClasses,
             Rank2Types,
             TypeOperators,
             OverloadedStrings #-}

-- | Functions for implementing the server side of JSON-RPC 2.0.
--   See <http://www.jsonrpc.org/specification>.
module Network.JsonRpc.Server (
                          -- ** Instructions
                          -- $instructions

                          -- ** Requests
                          -- $requests

                          -- ** Example
                          -- $example

                          -- ** Methods
                             RpcResult
                           , Method
                           , toMethod
                           , call
                           , callWithBatchStrategy
                           , Parameter(..)
                           , (:+:) (..)
                           , MethodParams
                          -- ** Errors
                           , RpcError (..)
                           , rpcError
                           , rpcErrorWithData
                           -- ** Deprecated
                           , Methods
                           , toMethods) where

import Network.JsonRpc.Types
import Data.Text (Text, append, pack)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.DeepSeq (NFData)
import Control.Monad (liftM, (<=<))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Except (runExceptT, throwError)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- $instructions
-- * Create methods by calling 'toMethod' and providing the method
--   names, lists of parameters, and functions to be called.
--
-- * Process a request by calling 'call' or 'callWithBatchStrategy'
--   on the 'Method's and input 'B.ByteString'.

-- $requests
-- This library handles by-name and by-position arguments, batch and
-- single requests, and notifications.  It also allows each
-- parameter of a method to be either optional (with a default value)
-- or required.  The function is called as long as all required
-- arguments are present.  A request providing more positional
-- arguments than the total number of optional and required
-- parameters to a function results in an error.  However, additional
-- by-name arguments are ignored.

-- $example
-- Here is an example with three JSON-RPC methods. It reads requests
-- from stdin and writes responses to stdout.  Compile it with the
-- build flag @demo@.
--   
-- > <insert Demo.hs>
--   

-- | Creates a method from a name, function, and parameter descriptions.
--   The parameter names must be unique.
toMethod :: (MethodParams f p m r, A.ToJSON r, Monad m) => Text -> f -> p -> Method m
toMethod name f params = let f' args = A.toJSON `liftM` _apply f params args
                         in Method name f'

type Methods m = [Method m]
{-# DEPRECATED Methods "Use ['Method' m]." #-}

toMethods :: [Method m] -> Methods m
toMethods = id
{-# DEPRECATED toMethods "Use 'call' directly." #-}

type MethodMap m = H.HashMap Text (Method m)

-- | Handles one JSON-RPC request. It is the same as
--   @callWithBatchStrategy sequence@.
call :: Monad m => [Method m]  -- ^ Choice of methods to call.
     -> B.ByteString           -- ^ JSON-RPC request.
     -> m (Maybe B.ByteString) -- ^ The response wrapped in 'Just', or
                               --   'Nothing' in the case of a notification,
                               --   all wrapped in the given monad.
call = callWithBatchStrategy sequence

-- | Handles one JSON-RPC request. The method names must be unique.
callWithBatchStrategy :: Monad m =>
                         (forall a . NFData a => [m a] -> m [a]) -- ^ Function specifying the
                                                                 --   evaluation strategy.
                      -> [Method m]                              -- ^ Choice of methods to call.
                      -> B.ByteString                            -- ^ JSON-RPC request.
                      -> m (Maybe B.ByteString)                  -- ^ The response wrapped in 'Just', or
                                                                 --   'Nothing' in the case of a notification,
                                                                 --   all wrapped in the given monad.
callWithBatchStrategy strategy methods =
    mthMap `seq` either returnErr callMethod . parse
  where
    mthMap = H.fromList $
             map (\mth@(Method name _) -> (name, mth)) methods
    parse :: B.ByteString -> Either RpcError (Either A.Value [A.Value])
    parse = runIdentity . runExceptT . parseVal <=< parseJson
    parseJson = maybe invalidJson return . A.decode
    parseVal val =
        case val of
          obj@(A.Object _) -> return $ Left obj
          A.Array vec | V.null vec -> throwInvalidRpc "Empty batch request"
                      | otherwise -> return $ Right $ V.toList vec
          _ -> throwInvalidRpc "Not a JSON object or array"
    callMethod rq =
        case rq of
          Left val -> encodeJust `liftM` singleCall mthMap val
          Right vals -> encodeJust `liftM` batchCall strategy mthMap vals
      where
        encodeJust r = A.encode <$> r
    returnErr = return . Just . A.encode . nullIdResponse
    invalidJson = throwError $ rpcError (-32700) "Invalid JSON"

singleCall :: Monad m => MethodMap m -> A.Value -> m (Maybe Response)
singleCall methods val = case parsed of
                                Left err -> return $ nullIdResponse err
                                Right (Request name args i) ->
                                  toResponse i `liftM` runExceptT (applyMethodTo args =<< method)
                                    where method = lookupMethod name methods
    where parsed = runIdentity $ runExceptT $ parseValue val
          applyMethodTo args (Method _ f) = f args

nullIdResponse :: RpcError -> Maybe Response
nullIdResponse err = toResponse (Just IdNull) (Left err :: Either RpcError ())

parseValue :: (A.FromJSON a, Monad m) => A.Value -> RpcResult m a
parseValue val = case A.fromJSON val of
                   A.Error msg -> throwInvalidRpc $ pack msg
                   A.Success x -> return x

lookupMethod :: Monad m => Text -> MethodMap m -> RpcResult m (Method m)
lookupMethod name = maybe notFound return . H.lookup name
    where notFound = throwError $ rpcError (-32601) $ "Method not found: " `append` name

throwInvalidRpc :: Monad m => Text -> RpcResult m a
throwInvalidRpc = throwError . rpcErrorWithData (-32600) "Invalid JSON-RPC 2.0 request"

batchCall :: Monad m => (forall a. NFData a => [m a] -> m [a])
          -> MethodMap m
          -> [A.Value]
          -> m (Maybe [Response])
batchCall strategy methods vals = (noNull . catMaybes) `liftM` results
    where results = strategy $ map (singleCall methods) vals
          noNull rs = if null rs then Nothing else Just rs

toResponse :: A.ToJSON a => Maybe Id -> Either RpcError a -> Maybe Response
toResponse (Just i) r = Just $ Response i $ A.toJSON <$> r
toResponse Nothing _ = Nothing

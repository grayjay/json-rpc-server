{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             TypeSynonymInstances,
             OverloadedStrings #-}

module Network.JsonRpc.Types ( RpcResult
                             , Method (..)
                             , Methods (..)
                             , Parameter(..)
                             , (:+:) (..)
                             , MethodParams (..)
                             , Request (..)
                             , Response (..)
                             , Id (..)
                             , RpcError
                             , rpcError
                             , rpcErrorWithData) where

import Data.String (fromString)
import Data.Maybe (catMaybes)
import Data.Text (Text, append, unpack)
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:), (.:?), (.!=))
import Data.Aeson.Types (emptyObject)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>), (<*>), (<|>), (*>), empty)
import Control.Monad (mplus, when)
import Control.Monad.Error (Error, ErrorT, throwError, strMsg, noMsg)
import Prelude hiding (length)

-- | Return type of a method. A method call can either fail with an 'RpcError'
--   or succeed with a result of type 'r'.
type RpcResult m r = ErrorT RpcError m r

-- | Parameter expected by a method.
data Parameter a
    -- | Required parameter with a name.
    = Required Text
    -- | Optional parameter with a name and default value.
    | Optional Text a

-- | A node in a type-level linked list of 'Parameter' types.  It is right associative.
data a :+: ps = (Parameter a) :+: ps
infixr :+:

-- | Relationship between a method's function ('f'), parameters ('p'),
--   monad ('m'), and return type ('r'). 'p' has one 'Parameter' for
--   every argument of 'f' and is terminated by @()@. The return type
--   of 'f' is @RpcResult m r@. This class is treated as closed.
class (Monad m, Functor m, A.ToJSON r) => MethodParams f p m r | f -> p m r where
    apply :: f -> p -> Args -> RpcResult m r

instance (Monad m, Functor m, A.ToJSON r) => MethodParams (RpcResult m r) () m r where
    apply _ _ (Right ar) | not $ V.null ar =
                             throwError $ rpcError (-32602) "Too many unnamed arguments"
    apply res _ _ = res

instance (A.FromJSON a, MethodParams f p m r) => MethodParams (a -> f) (a :+: p) m r where
    apply f (param :+: ps) args = arg >>= \a -> apply (f a) ps nextArgs
        where arg = either (parseArg name) return =<<
                    (Left <$> lookupValue) `mplus` (Right <$> paramDefault param)
              lookupValue = either (lookupArg name) (headArg name) args
              nextArgs = tailOrEmpty <$> args
              name = paramName param

lookupArg :: Monad m => Text -> A.Object -> RpcResult m A.Value
lookupArg name hm = case H.lookup name hm of
                      Nothing -> throwError $ missingArgError name
                      Just v -> return v

headArg :: Monad m => Text -> A.Array -> RpcResult m A.Value
headArg name vec | V.null vec = throwError $ missingArgError name
                 | otherwise = return $ V.head vec

tailOrEmpty :: A.Array -> A.Array
tailOrEmpty vec = if V.null vec then V.empty else V.tail vec

parseArg :: (Monad m, A.FromJSON r) => Text -> A.Value -> RpcResult m r
parseArg name val = case A.fromJSON val of
                      A.Error msg -> throwError $ argTypeError msg
                      A.Success x -> return x
    where argTypeError = rpcErrorWithData (-32602) $ "Wrong type for argument: " `append` name

paramDefault :: Monad m => Parameter a -> RpcResult m a
paramDefault (Optional _ d) = return d
paramDefault (Required name) = throwError $ missingArgError name

missingArgError :: Text -> RpcError
missingArgError name = rpcError (-32602) $ "Cannot find required argument: " `append` name

paramName :: Parameter a -> Text
paramName (Optional n _) = n
paramName (Required n) = n

-- | Single method.
data Method m = Method Text (Args -> RpcResult m A.Value)

-- | Multiple methods.
newtype Methods m = Methods (H.HashMap Text (Method m))

type Args = Either A.Object A.Array

data Request = Request Text Args (Maybe Id)

instance A.FromJSON Request where
    parseJSON (A.Object x) = (checkVersion =<< x .:? versionKey .!= jsonRpcVersion) *>
                           (Request <$>
                           x .: "method" <*>
                           (parseParams =<< x .:? "params" .!= emptyObject) <*>
                           parseId)
        where parseParams (A.Object obj) = return $ Left obj
              parseParams (A.Array ar) = return $ Right ar
              parseParams _ = empty
              checkVersion ver = when (ver /= jsonRpcVersion) $
                            fail $ "Wrong JSON RPC version: " ++ unpack ver
               -- (.:?) parses Null value as Nothing so parseId needs
               -- to use both (.:?) and (.:) to handle all cases
              parseId = x .:? idKey >>= \optional ->
                        case optional of
                          Nothing -> Just <$> (x .: idKey) <|> return Nothing
                          _ -> return optional
    parseJSON _ = empty

data Response = Response Id (Either RpcError A.Value)

instance A.ToJSON Response where
    toJSON (Response i result) = A.object pairs
        where pairs = [ versionKey .= jsonRpcVersion
                      , either ("error" .=) ("result" .=) result
                      , idKey .= i]

-- IdNumber cannot directly reference the type stored in A.Number,
-- since it changes between aeson-0.6 and 0.7.
data Id = IdString A.Value | IdNumber A.Value | IdNull

instance A.FromJSON Id where
    parseJSON x@(A.String _) = return $ IdString x
    parseJSON x@(A.Number _) = return $ IdNumber x
    parseJSON A.Null = return IdNull
    parseJSON _ = empty

instance A.ToJSON Id where
    toJSON (IdString x) = x
    toJSON (IdNumber x) = x
    toJSON IdNull = A.Null

-- | Error to be returned to the client.
data RpcError = RpcError Int Text (Maybe A.Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance A.ToJSON RpcError where
    toJSON (RpcError code msg data') = A.object pairs
        where pairs = catMaybes [ Just $ "code" .= code
                                , Just $ "message" .= msg
                                , ("data" .=) <$> data' ]

-- | Creates an 'RpcError' with the given error code and message.
--   According to the specification, server error codes should be
--   in the range -32099 to -32000, and application defined errors
--   should be outside the range -32768 to -32000.
rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

-- | Creates an 'RpcError' with the given code, message, and additional data.
--   See 'rpcError' for the recommended error code ranges.
rpcErrorWithData :: A.ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ A.toJSON errorData

jsonRpcVersion, versionKey, idKey :: Text
jsonRpcVersion = "2.0"
versionKey = "jsonrpc"
idKey = "id"

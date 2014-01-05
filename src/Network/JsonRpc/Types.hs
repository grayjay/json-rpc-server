{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             PatternGuards,
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
import Data.Aeson
import Data.Aeson.Types (emptyObject)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Data.Attoparsec.Number (Number)
import Control.Applicative ((<$>), (<*>), (<|>), (*>), empty)
import Control.Monad (when)
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
class (Monad m, Functor m, ToJSON r) => MethodParams f p m r | f -> p m r where
    apply :: f -> p -> Args -> RpcResult m r

instance (Monad m, Functor m, ToJSON r) => MethodParams (RpcResult m r) () m r where
    apply r _ args | Left _ <- args = r
                   | Right ar <- args, V.null ar = r
                   | otherwise = throwError $ rpcError (-32602) "Too many unnamed arguments"

instance (FromJSON a, MethodParams f p m r) => MethodParams (a -> f) (a :+: p) m r where
    apply f (param :+: ps) args = arg >>= \a -> apply (f a) ps nextArgs
        where arg = either (parseArg name) return =<<
                    (Left <$> lookupValue <|> Right <$> paramDefault param)
              lookupValue = either (lookupArg name) (headArg name) args
              nextArgs = tailOrEmpty <$> args
              name = paramName param

lookupArg :: Monad m => Text -> Object -> RpcResult m Value
lookupArg name hm = case H.lookup name hm of
                      Nothing -> throwError $ missingArgError name
                      Just v -> return v

headArg :: Monad m => Text -> V.Vector a -> RpcResult m a
headArg name vec | V.null vec = throwError $ missingArgError name
                 | otherwise = return $ V.head vec

tailOrEmpty :: V.Vector a -> V.Vector a
tailOrEmpty vec = if V.null vec then V.empty else V.tail vec

parseArg :: (Monad m, FromJSON r) => Text -> Value -> RpcResult m r
parseArg name val = case fromJSON val of
                      Error msg -> throwError $ rpcErrorWithData (-32602) ("Wrong type for argument: " `append` name) msg
                      Success x -> return x

paramDefault :: Monad m => Parameter a -> RpcResult m a
paramDefault (Optional _ d) = return d
paramDefault (Required name) = throwError $ missingArgError name

missingArgError :: Text -> RpcError
missingArgError name = rpcError (-32602) ("Cannot find required argument: " `append` name)

paramName :: Parameter a -> Text
paramName (Optional n _) = n
paramName (Required n) = n

-- | Single method.
data Method m = Method Text (Args -> RpcResult m Value)

-- | Multiple methods.
newtype Methods m = Methods (H.HashMap Text (Method m))

type Args = Either Object Array

data Request = Request Text Args (Maybe Id)

instance FromJSON Request where
    parseJSON (Object x) = (checkVersion =<< x .:? versionKey .!= jsonRpcVersion) *>
                           (Request <$>
                           x .: "method" <*>
                           (parseParams =<< x .:? "params" .!= emptyObject) <*>
                           (Just <$> x .: idKey <|> return Nothing)) -- (.:?) parses Null value as Nothing
        where parseParams (Object obj) = return $ Left obj
              parseParams (Array ar) = return $ Right ar
              parseParams _ = empty
              checkVersion ver = when (ver /= jsonRpcVersion) (fail $ "Wrong JSON RPC version: " ++ unpack ver)
    parseJSON _ = empty

data Response = Response Id (Either RpcError Value)

instance ToJSON Response where
    toJSON (Response i result) = object pairs
        where pairs = [ versionKey .= jsonRpcVersion
                      , either ("error" .=) ("result" .=) result
                      , idKey .= i]

data Id = IdString Text | IdNumber Number | IdNull

instance FromJSON Id where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull
    parseJSON _ = empty

instance ToJSON Id where
    toJSON i = case i of
                 IdString x -> String x
                 IdNumber x -> Number x
                 IdNull -> Null

-- | Error to be returned to the client.
data RpcError = RpcError Int Text (Maybe Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
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
rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

jsonRpcVersion, versionKey, idKey :: Text
jsonRpcVersion = "2.0"
versionKey = "jsonrpc"
idKey = "id"

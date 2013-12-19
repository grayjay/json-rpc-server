{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             UndecidableInstances,
             TypeOperators,
             OverloadedStrings #-}

module Data.JsonRpc.Types where

import Data.String
import Data.Text (Text, append, unpack)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types (Parser, emptyObject)
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

apply :: MethodParams f p m r => f -> p -> Args -> RpcResult m r
apply f p (Left hm) = mpApplyNamed f p hm
apply f p (Right vec) = mpApplyUnnamed f p vec

-- | Relationship between a method's function ('f'), parameters ('p'),
--   monad ('m'), and return type ('r'). 'p' has one 'Parameter' for
--   every argument of 'f' and is terminated by @()@. The return type
--   of 'f' is @RpcResult m r@. This class is treated as closed.
class (Monad m, Functor m, ToJSON r) => MethodParams f p m r | f -> p m r where
    mpApplyNamed :: f -> p -> Object -> RpcResult m r
    mpApplyUnnamed :: f -> p -> Array -> RpcResult m r

instance (Monad m, Functor m, ToJSON r) => MethodParams (RpcResult m r) () m r where
    mpApplyNamed r _ _ = r
    mpApplyUnnamed r _ args | V.null args = r
                            | otherwise = throwError $ rpcError (-32602) "Too many unnamed arguments"

instance (FromJSON a, MethodParams f p m r) => MethodParams (a -> f) (a :+: p) m r where
    mpApplyNamed = applyNamed
    mpApplyUnnamed = applyUnnamed

applyNamed :: (FromJSON a, MethodParams f p m r)
              => (a -> f)
              -> a :+: p
              -> Object
              -> RpcResult m r
applyNamed f (param :+: ps) args = arg >>= \a -> mpApplyNamed (f a) ps args
    where arg = (lookupM name args >>= parseArg name) <|> paramDefault param
          name = paramName param

lookupM :: Monad m => Text -> Object -> m Value
lookupM k hm = case H.lookup k hm of
                 Nothing -> fail "not in map"
                 Just v -> return v

applyUnnamed :: (FromJSON a, MethodParams f p m r)
              => (a -> f)
              -> a :+: p
              -> Array
              -> RpcResult m r
applyUnnamed f (param :+: ps) args = arg >>= \a -> mpApplyUnnamed (f a) ps (tailOrEmpty args)
    where arg = (headM args >>= parseArg name) <|> paramDefault param
          name = paramName param

headM :: Monad m => V.Vector a -> m a
headM vec | V.null vec = fail "empty vector"
          | otherwise = V.headM vec

tailOrEmpty :: V.Vector a -> V.Vector a
tailOrEmpty vec = if V.null vec then V.empty else V.tail vec

parseArg :: (Monad m, FromJSON r) => Text -> Value -> RpcResult m r
parseArg name val = case fromJSON val of
                      Error msg -> throwError $ rpcErrorWithData (-32602) ("Wrong type for argument: " `append` name) (Just msg)
                      Success x -> return x

paramDefault :: Monad m => Parameter a -> RpcResult m a
paramDefault (Optional _ d) = return d
paramDefault (Required name) = throwError $ RpcError (-32602) ("Cannot find required argument: " `append` name) Nothing

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
    parseJSON (Object x) = (x .:? versionKey >>= checkVersion) *>
                           (Request <$>
                           x .: methodKey <*>
                           (parseParams =<< x .:? paramsKey .!= emptyObject) <*>
                           x .:? idKey)
        where parseParams :: Value -> Parser Args
              parseParams val = withObject (unpack paramsKey) (return . Left) val <|>
                              withArray (unpack paramsKey) (return . Right) val
              checkVersion ver = let v = fromMaybe jsonRpcVersion ver
                                 in when (v /= jsonRpcVersion) (fail $ "Wrong JSON RPC version: " ++ unpack v)
    parseJSON _ = empty

data Response = Response { rspId :: Id
                         , rspResult :: Either RpcError Value }

instance ToJSON Response where
    toJSON r = object [versionKey .= jsonRpcVersion, result, "id" .= toJSON (rspId r)]
        where result = either (("error" .=) . toJSON) ("result" .=) (rspResult r)

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

-- | Error to be returned to the client.
data RpcError = RpcError Int Text (Maybe Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = [codeKey .= toJSON code, msgKey .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> [dataKey .= toJSON d]) data'

-- | Creates an 'RpcError' with the given error code and message.
--   Server error codes should be in the range -32000 to -32099.
rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

-- | Creates an 'RpcError' with the given code, message, and additional data.
--   Server error codes should be in the range -32000 to -32099.
rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

versionKey :: Text
versionKey = "jsonrpc"

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

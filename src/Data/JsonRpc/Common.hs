{-# LANGUAGE MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
, Rank2Types
, KindSignatures
, OverloadedStrings #-}

module Data.JsonRpc.Common where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as H
import Data.String (fromString)
import Data.Text (Text, append, unpack)
import Data.Attoparsec.Number (Number)
import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.Monad (mzero)
import Control.Monad.Error (ErrorT (..), Error, MonadError, strMsg, noMsg, ErrorT, throwError)

data Method f p (m :: * -> *) r = Method Text f p

method :: MethodParams f p m r => Text -> f -> p -> Method f p m r
method = Method

data Param a = Param Text (Maybe a)

type RpcResult m r = ErrorT RpcError m r

class Monad m => Function f m r | f -> m r where

instance Monad m => Function (RpcResult m r) m r where

instance (Monad m, Function f m r) => Function (a -> f) m r where

class (Function f m r, MethodParams f p m r, ToJSON r, FromJSON r) => ToClientFunction f p m r | f -> p m r, p m r -> f where
    toClientFunction :: (B.ByteString -> m B.ByteString) -> Text -> p -> (H.HashMap Text Value) -> f

instance (Monad m, ToJSON r, FromJSON r) => ToClientFunction (RpcResult m r) () m r where
    toClientFunction server mName _ hm = decode2 (server (encode' mName hm))

instance (ToClientFunction f p m r, ToJSON a, FromJSON a) => ToClientFunction (a -> f) (Param a, p) m r where
    toClientFunction server mName (Param name _, ps) hm arg = toClientFunction server mName ps (H.insert name (toJSON arg) hm)

toClientFunction' :: ToClientFunction f p m r => (B.ByteString -> m B.ByteString) -> Method f2 p m2 r -> f
toClientFunction' server (Method mName _ ps) = toClientFunction server mName ps H.empty

encode' :: Text -> H.HashMap Text Value -> B.ByteString
encode' mName hm = encode $ toJSON $ Request mName (Left hm) (Just $ IdNumber 100)

decode2 :: (Monad m, FromJSON r) => m B.ByteString -> RpcResult m r
decode2 x = ErrorT $ do
  str <- x
  let Just json = decode str
      Success (Response _ (Right response)) = fromJSON json
      parsed = fromJSON response
  case parsed of
    Error msg -> fail $ "message: " ++ msg
    Success val -> return $ Right val

class (Monad m, Function f m r) => MethodParams f p m r | f -> p m r where
    mpApply :: f -> p -> H.HashMap Text Value -> RpcResult m r

instance (ToJSON r, Monad m) => MethodParams (RpcResult m r) () m r where
    mpApply r _ _ = r

instance (FromJSON a, ToJSON r, MethodParams f p m r) => MethodParams (a -> f) (Param a, p)m r where
    mpApply = applyFunction

applyFunction :: (FromJSON a, MethodParams f p m r)
              => (a -> f)
              -> (Param a, p)
              -> H.HashMap Text Value
              -> RpcResult m r
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

data Response = Response { rspId :: Id
                         , rspResult :: Either RpcError Value }

instance ToJSON Response where
    toJSON r = object ["jsonrpc" .= jsonRpcVersion, result, "id" .= (toJSON $ rspId r)]
        where result = either (("error" .=) . toJSON) ("result" .=) (rspResult r)

instance FromJSON Response where
    parseJSON (Object r) = Response <$>
                           r .: "id" <*>
                           ((Left <$> r .: "error") <|> (Right <$> r .: "result"))
    parseJSON _ = empty

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

data RpcError = RpcError { errCode :: Int
                         , errMsg :: Text
                         , errData :: (Maybe Value)}
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = [codeKey .= toJSON code, msgKey .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> [dataKey .= toJSON d]) data'

instance FromJSON RpcError where
    parseJSON (Object err) = RpcError <$>
                             err .: codeKey <*>
                             err .: msgKey <*>
                             err .:? dataKey
    parseJSON _ = empty

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

data Request = Request { rqName :: Text
                       , rqParams :: Either Object Array
                       , rqId :: Maybe Id }

instance FromJSON Request where
    parseJSON (Object x) = Request <$>
                           x .: methodKey <*>
                           (parseParams =<< x .:? paramsKey .!= emptyObject) <*>
                           x .:? idKey
        where parseParams :: Value -> Parser (Either Object Array)
              parseParams = withObject (unpack paramsKey) (return . Left)
    parseJSON _ = mzero

instance ToJSON Request where
    toJSON (Request name (Left params) i) = object pairs
        where pairs = [ methodKey .= toJSON name
                      , paramsKey .= toJSON params
                      , idKey .= toJSON i ]

rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

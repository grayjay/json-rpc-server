{-# LANGUAGE MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
, Rank2Types
, OverloadedStrings #-}

module Data.JsonRpc.Common where

import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.String (fromString)
import Data.Text (Text, append)
import Data.Attoparsec.Number (Number)
import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.Monad (liftM, mzero)
import Control.Monad.Error (Error, strMsg, noMsg, ErrorT, lift, runErrorT, throwError)

data Method f p m r = Method f p

data Param a = Param Text (Maybe a)

type RpcResult m r = ErrorT RpcError m r

class Monad m => MethodParams f p m r | f -> p m r where
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

rpcError :: Int -> Text -> RpcError
rpcError code msg = RpcError code msg Nothing

rpcErrorWithData :: ToJSON a => Int -> Text -> a -> RpcError
rpcErrorWithData code msg errorData = RpcError code msg $ Just $ toJSON errorData

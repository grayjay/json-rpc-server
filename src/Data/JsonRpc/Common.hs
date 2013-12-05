{-# LANGUAGE OverloadedStrings #-}

module Data.JsonRpc.Common( RpcError (..)
                          , Response (..)
                          , Id (..)) where

import Data.Aeson
import Data.String (fromString)
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Control.Applicative ((<$>), (<*>), (<|>), empty)
import Control.Monad.Error (Error, strMsg, noMsg)

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

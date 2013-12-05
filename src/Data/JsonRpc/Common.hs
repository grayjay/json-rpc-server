{-# LANGUAGE OverloadedStrings #-}

module Data.JsonRpc.Common( RpcError (..)) where

import Data.Aeson
import Data.String (fromString)
import Data.Text (Text)
import Control.Monad.Error (Error, strMsg, noMsg)

data RpcError = RpcError Int Text (Maybe Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError (-32000) (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = ["code" .= toJSON code, "message" .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> ["data" .= toJSON d]) data'

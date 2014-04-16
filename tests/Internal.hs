{-# LANGUAGE OverloadedStrings,
             ExistentialQuantification #-}

module Internal ( request
                , request2_0
                , idRequest
                , successRsp
                , idSuccessRsp
                , errRsp
                , rpcErr
                , idErrRsp
                , fromJson
                , array
                , defaultId
                , errKey
                , dataKey
                , msgKey
                , resultKey
                , idKey
                , versionKey) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Text (Text)
import Control.Applicative ((<$>))

fromJson :: A.FromJSON a => A.Value -> Maybe a
fromJson v = case A.fromJSON v of
               A.Success x -> Just x
               _ -> Nothing

array :: [A.Value] -> A.Value
array = A.Array . V.fromList

idRequest :: Text -> Maybe A.Value -> A.Value
idRequest = request2_0 (Just defaultId)

request2_0 :: Maybe A.Value -> Text -> Maybe A.Value -> A.Value
request2_0 i = request (Just version) i . A.String

request :: Maybe Text -> Maybe A.Value -> A.Value -> Maybe A.Value -> A.Value
request ver i method args = A.object $ catMaybes [ Just $ "method" .= method
                                                 , ("params" .=) <$> args
                                                 , (idKey .=) <$> i
                                                 , (versionKey .=) <$> ver ]

idSuccessRsp :: A.Value -> A.Value
idSuccessRsp = successRsp defaultId

successRsp :: A.Value -> A.Value -> A.Value
successRsp i = response i "result"

idErrRsp :: Int -> A.Value
idErrRsp = errRsp defaultId

errRsp :: A.Value -> Int -> A.Value
errRsp i code = response i errKey $ rpcErr Nothing code ""

rpcErr :: Maybe A.Value -> Int -> Text -> A.Value
rpcErr d code msg = A.object $ ["code" .= code, msgKey .= msg] ++ dataPair 
    where dataPair = catMaybes [(dataKey .=) <$> d]

response :: A.Value -> Text -> A.Value -> A.Value
response i key result = A.object [idKey .= i, key .= result, versionKey .= version]

defaultId :: A.Value
defaultId = A.Number 3

versionKey :: Text
versionKey = "jsonrpc"

idKey :: Text
idKey = "id"

resultKey :: Text
resultKey = "result"

errKey :: Text
errKey = "error"

msgKey :: Text
msgKey = "message"

dataKey :: Text
dataKey = "data"

version :: Text
version = "2.0"

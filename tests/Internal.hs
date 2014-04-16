{-# LANGUAGE OverloadedStrings,
             ExistentialQuantification #-}

module Internal ( request
                , errRsp
                , rpcErr
                , defaultIdErrRsp
                , nullIdErrRsp
                , fromJson
                , array
                , defaultRq
                , defaultRsp
                , method
                , params
                , id'
                , version
                , result
                , defaultId
                , defaultResult
                , errKey
                , dataKey
                , msgKey
                , resultKey
                , idKey
                , versionKey) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.HashMap.Strict as H
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

defaultRq :: A.Value
defaultRq = request (Just defaultId) "subtract" args
    where args = Just $ A.object ["x" .= A.Number 1, "y" .= A.Number 2]

method :: A.Value -> Text -> A.Value
method rq m = insert rq "method" $ Just $ A.String m

params :: A.Value -> Maybe A.Value -> A.Value
params rq = insert rq "params"

id' :: A.Value -> Maybe A.Value -> A.Value
id' rq = insert rq "id"

version :: A.Value -> Maybe A.Value -> A.Value
version rq = insert rq "jsonrpc"

request :: Maybe A.Value -> Text -> Maybe A.Value -> A.Value
request i m args = A.object $ catMaybes [ Just $ "method" .= A.String m
                                        , ("params" .=) <$> args
                                        , (idKey .=) <$> i
                                        , Just (versionKey .= defaultVersion)]

defaultRsp :: A.Value
defaultRsp = response defaultId "result" defaultResult

result :: A.Value -> A.Value -> A.Value
result rsp = insert rsp "result" . Just

defaultIdErrRsp :: Int -> A.Value
defaultIdErrRsp = errRsp defaultId

nullIdErrRsp :: Int -> A.Value
nullIdErrRsp = errRsp A.Null

errRsp :: A.Value -> Int -> A.Value
errRsp i code = response i errKey $ rpcErr Nothing code ""

rpcErr :: Maybe A.Value -> Int -> Text -> A.Value
rpcErr d code msg = A.object $ ["code" .= code, msgKey .= msg] ++ dataPair 
    where dataPair = catMaybes [(dataKey .=) <$> d]

response :: A.Value -> Text -> A.Value -> A.Value
response i key res = A.object [idKey .= i, key .= res, versionKey .= defaultVersion]

insert :: A.Value -> Text -> Maybe A.Value -> A.Value
insert (A.Object obj) key Nothing = A.Object $ H.delete key obj
insert (A.Object obj) key (Just val) = A.Object $ H.insert key val obj
insert v _ _ = v

defaultId :: A.Value
defaultId = A.Number 3

defaultResult :: A.Value
defaultResult = A.Number (-1)

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

defaultVersion :: Text
defaultVersion = "2.0"

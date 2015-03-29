{-# LANGUAGE CPP,
             OverloadedStrings #-}

module Internal ( request
                , errRsp
                , rpcErr
                , defaultIdErrRsp
                , nullIdErrRsp
                , array
                , rspToIdString
                , defaultRq
                , defaultRsp
                , method
                , params
                , id'
                , version
                , result) where

import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Text (Text)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

array :: [A.Value] -> A.Value
array = A.Array . V.fromList

rspToIdString :: A.Value -> Maybe String
rspToIdString (A.Object rsp) = show <$> H.lookup "id" rsp
rspToIdString _ = Nothing

request :: Maybe A.Value -> Text -> Maybe A.Value -> A.Value
request i m args = A.object $ catMaybes [ Just $ "method" .= A.String m
                                        , ("params" .=) <$> args
                                        , ("id" .=) <$> i
                                        , Just ("jsonrpc" .= A.String "2.0")]

defaultRq :: A.Value
defaultRq = request (Just defaultId) "subtract" args
    where args = Just $ A.object ["x" .= A.Number 1, "y" .= A.Number 2]

response :: A.Value -> Text -> A.Value -> A.Value
response i key res = A.object ["id" .= i, key .= res, "jsonrpc" .= A.String "2.0"]

defaultRsp :: A.Value
defaultRsp = response defaultId "result" defaultResult

defaultIdErrRsp :: Int -> A.Value
defaultIdErrRsp = errRsp defaultId

nullIdErrRsp :: Int -> A.Value
nullIdErrRsp = errRsp A.Null

errRsp :: A.Value -> Int -> A.Value
errRsp i code = response i "error" $ rpcErr Nothing code ""

rpcErr :: Maybe A.Value -> Int -> Text -> A.Value
rpcErr d code msg = A.object $ ["code" .= code, "message" .= msg] ++ dataPair 
    where dataPair = catMaybes [("data" .=) <$> d]

method :: A.Value -> Text -> A.Value
method rq m = insert rq "method" $ Just $ A.String m

params :: A.Value -> Maybe A.Value -> A.Value
params rq = insert rq "params"

id' :: A.Value -> Maybe A.Value -> A.Value
id' rq = insert rq "id"

version :: A.Value -> Maybe A.Value -> A.Value
version rq = insert rq "jsonrpc"

result :: A.Value -> A.Value -> A.Value
result rsp = insert rsp "result" . Just

insert :: A.Value -> Text -> Maybe A.Value -> A.Value
insert (A.Object obj) key Nothing = A.Object $ H.delete key obj
insert (A.Object obj) key (Just val) = A.Object $ H.insert key val obj
insert v _ _ = v

defaultId :: A.Value
defaultId = A.Number 3

defaultResult :: A.Value
defaultResult = A.Number (-1)

{-# LANGUAGE MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
, KindSignatures
, OverloadedStrings #-}

module JsonRpcServer ( RpcResult(..)
                     , RpcError(..)
                     , Param(..)
                     , MethodParams
                     , JsonFunction
                     , toJsonFunction
                     , JsonFunctions
                     , toJsonFunctions
                     , call
                     , liftR) where

import Data.Text hiding (map)
import Data.String
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Prelude hiding (length)

data RpcError = RpcError Int Text (Maybe Value)
              deriving Show

instance Error RpcError where
    noMsg = strMsg "unknown error"
    strMsg msg = RpcError 32000 (fromString msg) Nothing

instance ToJSON RpcError where
    toJSON (RpcError code msg data') = object pairs
        where pairs = ["code" .= toJSON code, "message" .= toJSON msg] ++ dataPair
              dataPair = maybe [] (\d -> ["data" .= toJSON d]) data'

type RpcResult m a = ErrorT RpcError m a

data Param a = Param Text (Maybe a)
             deriving Show

data Method (m :: * -> *) a p = Method Text a p

class Monad m => MethodParams m a p | a -> m p where
    mpApply :: a -> p -> H.HashMap Text Value -> RpcResult m Value

instance (ToJSON a, Monad m) => MethodParams m (RpcResult m a) () where
    mpApply r _ _ = liftM toJSON r

instance (FromJSON a, Monad m, MethodParams m b p) => MethodParams m (a -> b) (Param a, p) where
    mpApply = applyFunction

applyFunction :: (FromJSON a, Monad m, MethodParams m b p) => (a -> b)
              -> (Param a, p)
              -> H.HashMap Text Value
              -> RpcResult m Value
applyFunction f ((Param pName d), ps) args = let arg = maybe d fromJSON' $ H.lookup pName args
                                                 fromJSON' x = case fromJSON x of
                                                                 Error _ -> Nothing
                                                                 Success val -> Just val
                                             in case arg of
                                                  Nothing -> fail "Cannot find required argument"
                                                  Just arg' -> mpApply (f arg') ps args

method :: (Monad m, MethodParams m a p) => Text -> a -> p -> Method m a p
method = Method

data Request = Request { rqName :: Text
                       , rqParams :: Either Object Array
                       , rqId :: Maybe Id }

instance FromJSON Request where
    parseJSON (Object x) = Request <$>
                           x .: "method" <*>
                           (parseParams =<< x .:? "params" .!= emptyObject) <*>
                           x .:? "id"
        where parseParams :: Value -> Parser (Either Object Array)
              parseParams = withObject "params" (return . Left)

data Id = IdString Text | IdNumber Number | IdNull

instance FromJSON Id where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull

instance ToJSON Id where
    toJSON id = case id of
                  IdString x -> toJSON x
                  IdNumber x -> toJSON x
                  IdNull -> Null

data Response = Response { rspResult :: Either RpcError Value
                         , rspId :: Id }

instance ToJSON Response where
    toJSON r = object ["jsonrpc" .= jsonRpcVersion, result, "id" .= (toJSON $ rspId r)]
        where result = either (("error" .=) . toJSON) ("result" .=) (rspResult r)

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

data JsonFunction m = JsonFunction Text (H.HashMap Text Value -> RpcResult m Value)

newtype JsonFunctions m = JsonFunctions (H.HashMap Text (JsonFunction m))

toJsonFunction :: (Monad m, MethodParams m a p) => Text -> a -> p -> JsonFunction m
toJsonFunction name f params = toJsonFunction' $ method name f params

toJsonFunction' :: (Monad m, MethodParams m a p) => Method m a p -> JsonFunction m
toJsonFunction' (Method name f params) = JsonFunction name $ mpApply f params

toJsonFunctions :: [JsonFunction m] -> JsonFunctions m
toJsonFunctions fs = JsonFunctions $ H.fromList $ map (\f@(JsonFunction n _) -> (n, f)) fs

call :: Monad m => JsonFunctions m -> Value -> m Value
call (JsonFunctions fs) x = (liftM $ (toJSON . toResponse id)) $ runErrorT $ f params
    where Just (JsonFunction _ f) = H.lookup name fs
          params :: H.HashMap Text Value
          Success (Request name (Left params) id) = fromJSON x

toResponse :: Maybe Id -> Either RpcError Value -> Maybe Response
toResponse Nothing _ = Nothing
toResponse (Just id) r = Just $ Response r id

liftR :: (Monad m) => m a -> RpcResult m a
liftR = lift

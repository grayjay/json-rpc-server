{-# LANGUAGE OverloadedStrings #-}

module TestTypes ( TestRequest (..)
                 , TestResponse (..)
                 , TestRpcError (..)
                 , TestId (..)
                 , jsonRpcVersion
                 , versionKey) where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Data.HashMap.Strict (size)
import Control.Applicative ((<$>), (<*>), (<|>), pure, empty)
import Control.Monad (when, guard)

data TestRpcError = TestRpcError { errCode :: Int
                                 , errMsg :: Text
                                 , errData :: Maybe Value}
                    deriving (Eq, Show)

instance FromJSON TestRpcError where
    parseJSON (Object obj) = do
      d <- obj .:? "data"
      when (size obj /= maybe 2 (const 3) d) $ fail "Wrong number of keys"
      TestRpcError <$> obj .: "code" <*> obj .: "message" <*> pure d
    parseJSON _ = empty

data TestRequest = TestRequest Text (Maybe (Either Object Array)) (Maybe TestId)

instance ToJSON TestRequest where
    toJSON (TestRequest name params i) = object pairs
        where pairs = catMaybes [Just $ "method" .= name, idPair, paramsPair]
              idPair = ("id" .=) <$> i
              paramsPair = either toPair toPair <$> params
                  where toPair v = "params" .= v

data TestResponse = TestResponse { rspId :: TestId
                                 , rspResult :: Either TestRpcError Value }
                    deriving (Eq, Show)

instance FromJSON TestResponse where
    parseJSON (Object obj) = do
      guard (size obj == 3)
      guard . (jsonRpcVersion ==) =<< obj .: versionKey
      TestResponse <$> obj .: "id" <*>
        ((Left <$> obj .: "error") <|> (Right <$> obj .: "result"))
    parseJSON _ = empty

data TestId = IdString Text | IdNumber Number | IdNull
              deriving (Eq, Show)

instance FromJSON TestId where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull
    parseJSON _ = empty

instance ToJSON TestId where
    toJSON i = case i of
                 IdString x -> String x
                 IdNumber x -> Number x
                 IdNull -> Null

versionKey :: Text
versionKey = "jsonrpc"

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

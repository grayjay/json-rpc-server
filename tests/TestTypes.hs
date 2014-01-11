{-# LANGUAGE OverloadedStrings #-}

module TestTypes ( TestRequest (..)
                 , TestResponse (..)
                 , TestRpcError (..)
                 , TestId
                 , idString
                 , idNumber
                 , idNull
                 , versionKey) where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:), (.:?))
import Data.Maybe (catMaybes)
import Data.String (IsString, fromString)
import Data.Text (Text, pack)
import Data.HashMap.Strict (size)
import Control.Applicative ((<$>), (<*>), (<|>), pure, empty)
import Control.Monad (when, guard)

data TestRpcError = TestRpcError { errCode :: Int
                                 , errMsg :: Text
                                 , errData :: Maybe A.Value}
                    deriving (Eq, Show)

instance A.FromJSON TestRpcError where
    parseJSON (A.Object obj) = do
      d <- obj .:? "data"
      when (size obj /= maybe 2 (const 3) d) $ fail "Wrong number of keys"
      TestRpcError <$> obj .: "code" <*> obj .: "message" <*> pure d
    parseJSON _ = empty

data TestRequest = TestRequest Text (Maybe (Either A.Object A.Array)) (Maybe TestId)

instance A.ToJSON TestRequest where
    toJSON (TestRequest name params i) = A.object pairs
        where pairs = catMaybes [Just $ "method" .= name, idPair, paramsPair]
              idPair = ("id" .=) <$> i
              paramsPair = either toPair toPair <$> params
                  where toPair v = "params" .= v

data TestResponse = TestResponse { rspId :: TestId
                                 , rspResult :: Either TestRpcError A.Value }
                    deriving (Eq, Show)

instance A.FromJSON TestResponse where
    parseJSON (A.Object obj) = do
      guard (size obj == 3)
      guard . (pack "2.0" ==) =<< obj .: versionKey
      TestResponse <$> obj .: "id" <*>
        ((Left <$> obj .: "error") <|> (Right <$> obj .: "result"))
    parseJSON _ = empty

data TestId = IdString A.Value | IdNumber A.Value | IdNull
              deriving (Eq, Show)

idString :: String -> TestId
idString = IdString . A.String . fromString

idNumber :: Integer -> TestId
idNumber = IdNumber . A.Number . fromInteger

idNull :: TestId
idNull = IdNull

instance A.FromJSON TestId where
    parseJSON x@(A.String _) = return $ IdString x
    parseJSON x@(A.Number _) = return $ IdNumber x
    parseJSON A.Null = return IdNull
    parseJSON _ = empty

instance A.ToJSON TestId where
    toJSON (IdString x) = x
    toJSON (IdNumber x) = x
    toJSON IdNull = A.Null

versionKey :: Text
versionKey = "jsonrpc"

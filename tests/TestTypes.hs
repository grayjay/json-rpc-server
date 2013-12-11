{-# LANGUAGE OverloadedStrings #-}

module TestTypes ( TestResponse (..)
                 , TestRpcError (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Data.HashMap.Strict (size)
import Control.Applicative

data TestRpcError = TestRpcError { errCode :: Int
                                 , errMsg :: Text
                                 , errData :: (Maybe Value)}
                    deriving Show

instance FromJSON TestRpcError where
    parseJSON (Object err) = TestRpcError <$>
                             err .: codeKey <*>
                             err .: msgKey <*>
                             err .:? dataKey
    parseJSON _ = empty

data TestResponse = TestResponse { rspId :: TestId
                                 , rspResult :: Either TestRpcError Value }

instance FromJSON TestResponse where
    parseJSON (Object r) = failIf (size r /= 3) *> checkVersion *>
                           (TestResponse <$>
                           r .: "id" <*>
                           ((Left <$> r .: "error") <|> (Right <$> r .: "result")))
        where checkVersion = r .: versionKey >>= failIf . (/= jsonRpcVersion)
    parseJSON _ = empty

failIf :: Bool -> Parser ()
failIf b = if b then empty else pure ()

data TestId = IdString Text | IdNumber Number | IdNull

instance FromJSON TestId where
    parseJSON (String x) = return $ IdString x
    parseJSON (Number x) = return $ IdNumber x
    parseJSON Null = return IdNull
    parseJSON _ = empty

instance ToJSON TestId where
    toJSON i = case i of
                 IdString x -> toJSON x
                 IdNumber x -> toJSON x
                 IdNull -> Null

versionKey :: Text
versionKey = "jsonrpc"

jsonRpcVersion :: Text
jsonRpcVersion = "2.0"

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
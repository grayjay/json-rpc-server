{-# LANGUAGE OverloadedStrings #-}

module TestTypes ( TestRequest (..)
                 , TestResponse (..)
                 , TestRpcError (..)
                 , TestId (..)
                 , jsonRpcVersion) where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Data.HashMap.Strict (size)
import Control.Applicative

data TestRpcError = TestRpcError { errCode :: Int
                                 , errMsg :: Text
                                 , errData :: Maybe Value}
                    deriving (Eq, Show)

instance FromJSON TestRpcError where
    parseJSON (Object err) = (TestRpcError <$>
                             err .: "code" <*>
                             err .: "message" <*>
                             err .:? "data") >>= checkKeys
        where checkKeys e = let checkSize s = failIf $ size err /= s
                            in pure e <* checkSize (case errData e of
                                                        Nothing -> 2
                                                        Just _ -> 3)
    parseJSON _ = empty

data TestRequest = TestRequest Text (Maybe (Either Object Array)) (Maybe TestId)

instance ToJSON TestRequest where
    toJSON (TestRequest name params i) = object pairs
        where pairs = [ "method" .= toJSON name
                      , "id" .= toJSON i ] ++ ps
              ps = case params of
                         Nothing -> []
                         Just (Left named) -> paramsPair named
                         Just (Right unnamed) -> paramsPair unnamed
                  where paramsPair v = ["params" .= toJSON v]

data TestResponse = TestResponse { rspId :: TestId
                                 , rspResult :: Either TestRpcError Value }
                    deriving (Eq, Show)

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
              deriving (Eq, Show)

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

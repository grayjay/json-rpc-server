{-# LANGUAGE OverloadedStrings #-}

import Data.JsonRpc.Common
import Data.JsonRpc.Server
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative
import Control.Monad.Identity
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testCase "invalid JSON" testInvalidJson
                   , testCase "invalid JSON RPC" testInvalidJsonRpc]

emptyJsonFunctions :: Monad m => JsonFunctions m
emptyJsonFunctions = toJsonFunctions []

testInvalidJson :: Assertion
testInvalidJson = (getErrorCode =<< runIdentity result) @?= Just (-32700)
    where result = call emptyJsonFunctions "5"

testInvalidJsonRpc :: Assertion
testInvalidJsonRpc = (getErrorCode =<< runIdentity result) @?= Just (-32600)
    where result = call emptyJsonFunctions $ encode $ object ["id" .= (10 :: Int)]

fromByteString x = case fromJSON <$> decode x of
                     Just (Success x') -> Just x'
                     _ -> Nothing

getErrorCode :: B.ByteString -> Maybe Int
getErrorCode b = fromByteString b >>= \r ->
                 case r of
                   Just (Response _ (Left (RpcError code _ _))) -> Just code
                   _ -> Nothing

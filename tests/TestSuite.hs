{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.JsonRpc.Server
import TestTypes
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative
import Control.Monad.Identity
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testCase "invalid JSON" testInvalidJson
                   , testCase "invalid JSON RPC" testInvalidJsonRpc
                   , testCase "empty batch call" testInvalidBatchCall]

testInvalidJson :: Assertion
testInvalidJson = checkErrorCode "5" (-32700)

testInvalidJsonRpc :: Assertion
testInvalidJsonRpc = checkErrorCode (encode $ object ["id" .= (10 :: Int)]) (-32600)

testInvalidBatchCall :: Assertion
testInvalidBatchCall = checkErrorCode (encode $ emptyArray) (-32600)

checkErrorCode :: B.ByteString -> Int -> Assertion
checkErrorCode val expectedCode = (getErrorCode =<< runIdentity result) @?= Just expectedCode
    where result = call emptyJsonFunctions val

fromByteString :: FromJSON a => B.ByteString -> Maybe a
fromByteString x = case fromJSON <$> decode x of
                     Just (Success x') -> Just x'
                     _ -> Nothing

emptyJsonFunctions :: Monad m => JsonFunctions m
emptyJsonFunctions = toJsonFunctions []

getErrorCode :: B.ByteString -> Maybe Int
getErrorCode b = fromByteString b >>= \r ->
                 case r of
                   Just (TestResponse _ (Left (TestRpcError code _ _))) -> Just code
                   _ -> Nothing

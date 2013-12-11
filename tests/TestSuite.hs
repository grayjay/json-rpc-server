{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.JsonRpc.Server
import TestTypes
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad.Identity
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testCase "invalid JSON" testInvalidJson
                   , testCase "invalid JSON RPC" testInvalidJsonRpc
                   , testCase "empty batch call" testInvalidBatchCall
                   , testCase "wrong version in request" testWrongVersion
                   , testCase "method not found" testMethodNotFound
                   , testCase "missing required argument" testMissingRequiredArg
                   , testCase "invalid notification" testNoResponseToInvalidNotification
                   , testCase "no arguments" testNoArgs
                   , testCase "empty argument array" testEmptyUnnamedArgs
                   , testCase "empty argument object" testEmptyNamedArgs
                   , testCase "allow extra named argument" testAllowExtraNamedArg
                   , testCase "use default argument" testDefaultArg ]

testInvalidJson :: Assertion
testInvalidJson = checkErrorCode "5" (-32700)

testInvalidJsonRpc :: Assertion
testInvalidJsonRpc = checkErrorCode (encode $ object ["id" .= (10 :: Int)]) (-32600)

testInvalidBatchCall :: Assertion
testInvalidBatchCall = checkErrorCode (encode emptyArray) (-32600)

testWrongVersion :: Assertion
testWrongVersion = checkErrorCode (encode requestWrongVersion) (-32600)
    where requestWrongVersion = Object $ H.insert jsonRpcVersion (String "1") hm
          Object hm = toJSON $ addRequest [("a1", Number 4)] (IdNumber 10)

testMethodNotFound :: Assertion
testMethodNotFound = checkErrorCode (encode request) (-32601)
    where request = TestRequest "ad" Nothing (Just $ IdNumber 3)

testMissingRequiredArg :: Assertion
testMissingRequiredArg = checkErrorCode (encode request) (-32602)
    where request = addRequest [("a2", Number 20)] (IdNumber 2)

testNoResponseToInvalidNotification :: Assertion
testNoResponseToInvalidNotification = runIdentity response @?= Nothing
    where response = call (toJsonFunctions [addMethod]) $ encode request
          request = TestRequest "add2" Nothing Nothing

testAllowExtraNamedArg :: Assertion
testAllowExtraNamedArg = (fromByteString $ fromJust $ runIdentity response) @?= (Just $ TestResponse i (Right $ Number 30))
    where response = call (toJsonFunctions [addMethod]) $ encode request
          request = addRequest args i
          args = [("a1", Number 10), ("a2", Number 20), ("a3", String "extra")]
          i = IdString "ID"

testDefaultArg :: Assertion
testDefaultArg = (fromByteString $ fromJust $ runIdentity response) @?= (Just $ TestResponse i (Right $ Number 1000))
    where response = call (toJsonFunctions [addMethod]) $ encode request
          request = addRequest args i
          args = [("a", Number 500), ("a1", Number 1000)]
          i = IdNumber 3

testNoArgs :: Assertion
testNoArgs = compareGetTimeResult Nothing

testEmptyUnnamedArgs :: Assertion
testEmptyUnnamedArgs = compareGetTimeResult $ Just $ Right $ empty

testEmptyNamedArgs :: Assertion
testEmptyNamedArgs = compareGetTimeResult $ Just $ Left $ H.empty

compareGetTimeResult :: Maybe (Either Object Array) -> Assertion
compareGetTimeResult requestArgs = assertEqual "unexpected rpc response" expected =<<
                                   ((fromByteString . fromJust) <$> call (toJsonFunctions [getTimeMethod]) (encode getTimeRequest))
    where expected = Just $ TestResponse i (Right $ Number 100)
          getTimeRequest = TestRequest "get_time_seconds" requestArgs (Just i)
          i = IdString "Id 1"

addRequest :: [(Text, Value)] -> TestId -> TestRequest
addRequest args i = TestRequest "add" (Just $ Left $ H.fromList args) (Just i)

checkErrorCode :: B.ByteString -> Int -> Assertion
checkErrorCode val expectedCode = (getErrorCode =<< runIdentity result) @?= Just expectedCode
    where result = call (toJsonFunctions [addMethod]) val

fromByteString :: FromJSON a => B.ByteString -> Maybe a
fromByteString x = case fromJSON <$> decode x of
                     Just (Success x') -> Just x'
                     _ -> Nothing

getErrorCode :: B.ByteString -> Maybe Int
getErrorCode b = fromByteString b >>= \r ->
                 case r of
                   Just (TestResponse _ (Left (TestRpcError code _ _))) -> Just code
                   _ -> Nothing

addMethod :: JsonFunction Identity
addMethod = toJsonFunction "add" (\x y -> liftToResult $ Identity (x + y :: Int))
            (Param "a1" Nothing, (Param "a2" (Just 0), ()))

getTimeMethod :: JsonFunction IO
getTimeMethod = toJsonFunction "get_time_seconds" (liftToResult getTestTime) ()

getTestTime :: IO Integer
getTestTime = return 100

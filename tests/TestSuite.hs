{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.JsonRpc.Server
import TestTypes
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad.Trans
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
                   , testCase "disallow extra unnamed arguments" testDisallowExtraUnnamedArg
                   , testCase "invalid notification" testNoResponseToInvalidNotification
                   , testCase "allow missing version" testAllowMissingVersion
                   , testCase "no arguments" testNoArgs
                   , testCase "empty argument array" testEmptyUnnamedArgs
                   , testCase "empty argument object" testEmptyNamedArgs
                   , testCase "allow extra named argument" testAllowExtraNamedArg
                   , testCase "use default named argument" testDefaultNamedArg
                   , testCase "use default unnamed argument" testDefaultUnnamedArg
                   , testCase "null request ID" testNullId ]

testInvalidJson :: Assertion
testInvalidJson = checkErrorCodeWithSubtract "5" (-32700)

testInvalidJsonRpc :: Assertion
testInvalidJsonRpc = checkErrorCodeWithSubtract (encode $ object ["id" .= (10 :: Int)]) (-32600)

testInvalidBatchCall :: Assertion
testInvalidBatchCall = checkErrorCodeWithSubtract (encode emptyArray) (-32600)

testWrongVersion :: Assertion
testWrongVersion = checkErrorCodeWithSubtract (encode requestWrongVersion) (-32600)
    where requestWrongVersion = Object $ H.insert versionKey (String "1") hm
          Object hm = toJSON $ subtractRequestNamed [("a1", Number 4)] (IdNumber 10)

testMethodNotFound :: Assertion
testMethodNotFound = checkErrorCodeWithSubtract (encode request) (-32601)
    where request = TestRequest "ad" Nothing (Just $ IdNumber 3)

testMissingRequiredArg :: Assertion
testMissingRequiredArg = checkErrorCodeWithSubtract (encode request) (-32602)
    where request = subtractRequestNamed [("a2", Number 20)] (IdNumber 2)

testDisallowExtraUnnamedArg :: Assertion
testDisallowExtraUnnamedArg = checkErrorCodeWithSubtract (encode request) (-32602)
    where request = subtractRequestUnnamed (map Number [1, 2, 3]) (IdString "i")

testNoResponseToInvalidNotification :: Assertion
testNoResponseToInvalidNotification = runIdentity response @?= Nothing
    where response = call (toMethods [subtractMethod]) $ encode request
          request = TestRequest "subtract2" Nothing Nothing

testAllowMissingVersion :: Assertion
testAllowMissingVersion = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 1))
    where requestNoVersion = Object $ H.delete versionKey hm
          Object hm = toJSON $ subtractRequestNamed [("a1", Number 1)] i
          response = call (toMethods [subtractMethod]) $ encode requestNoVersion
          i = IdNumber (-1)

testAllowExtraNamedArg :: Assertion
testAllowExtraNamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number (-10)))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args i
          args = [("a1", Number 10), ("a2", Number 20), ("a3", String "extra")]
          i = IdString "ID"

testDefaultNamedArg :: Assertion
testDefaultNamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 1000))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args i
          args = [("a", Number 500), ("a1", Number 1000)]
          i = IdNumber 3

testDefaultUnnamedArg :: Assertion
testDefaultUnnamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 4))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestUnnamed [Number 4] i
          i = IdNumber 0

testNullId :: Assertion
testNullId = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse IdNull (Right $ Number 60))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args IdNull
          args = [("a2", Number 70), ("a1", Number (-10))]

testNoArgs :: Assertion
testNoArgs = compareGetTimeResult Nothing

testEmptyUnnamedArgs :: Assertion
testEmptyUnnamedArgs = compareGetTimeResult $ Just $ Right empty

testEmptyNamedArgs :: Assertion
testEmptyNamedArgs = compareGetTimeResult $ Just $ Left H.empty

compareGetTimeResult :: Maybe (Either Object Array) -> Assertion
compareGetTimeResult requestArgs = assertEqual "unexpected rpc response" expected =<<
                                   ((fromByteString . fromJust) <$> call (toMethods [getTimeMethod]) (encode getTimeRequest))
    where expected = Just $ TestResponse i (Right $ Number 100)
          getTimeRequest = TestRequest "get_time_seconds" requestArgs (Just i)
          i = IdString "Id 1"

subtractRequestNamed :: [(Text, Value)] -> TestId -> TestRequest
subtractRequestNamed args i = TestRequest "subtract" (Just $ Left $ H.fromList args) (Just i)

subtractRequestUnnamed :: [Value] -> TestId -> TestRequest
subtractRequestUnnamed args i = TestRequest "subtract" (Just $ Right $ V.fromList args) (Just i)

checkErrorCodeWithSubtract :: B.ByteString -> Int -> Assertion
checkErrorCodeWithSubtract val expectedCode = (getErrorCode =<< runIdentity result) @?= Just expectedCode
    where result = call (toMethods [subtractMethod]) val

fromByteString :: FromJSON a => B.ByteString -> Maybe a
fromByteString x = case fromJSON <$> decode x of
                     Just (Success x') -> Just x'
                     _ -> Nothing

getErrorCode :: B.ByteString -> Maybe Int
getErrorCode b = fromByteString b >>= \r ->
                 case r of
                   Just (TestResponse _ (Left (TestRpcError code _ _))) -> Just code
                   _ -> Nothing

subtractMethod :: Method Identity
subtractMethod = toMethod "subtract" sub (Required "a1" :+: Optional "a2" 0 :+: ())
            where sub :: Int -> Int -> RpcResult Identity Int
                  sub x y = return (x - y)

getTimeMethod :: Method IO
getTimeMethod = toMethod "get_time_seconds" getTime ()
    where getTime :: RpcResult IO Integer
          getTime = liftIO getTestTime

getTestTime :: IO Integer
getTestTime = return 100

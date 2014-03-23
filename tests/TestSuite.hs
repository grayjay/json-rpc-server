{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JsonRpc.Server
import TestTypes
import qualified TestParallelism as P
import Data.Maybe
import Data.List (sortBy)
import Data.Function (on)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testCase "encode RPC error" testEncodeRpcError

                   , testCase "encode error with data" testEncodeErrorWithData

                   , testCase "invalid JSON" $
                         assertSubtractResponse ("5" :: String) (errResponse idNull (-32700))

                   , testCase "invalid JSON RPC" $
                         assertSubtractResponse (object ["id" .= (10 :: Int)]) (errResponse idNull (-32600))

                   , testCase "empty batch call" $
                         assertSubtractResponse emptyArray (errResponse idNull (-32600))

                   , testCase "invalid batch element" testInvalidBatchElement

                   , testCase "wrong version in request" testWrongVersion

                   , let req = TestRequest "add" (Just defaultArgs) (Just nonNullId)
                         rsp = errResponse nonNullId (-32601)
                     in testCase "method not found" $ assertSubtractResponse req rsp

                   , let req = TestRequest "Subtract" (Just defaultArgs) (Just nonNullId)
                         rsp = errResponse nonNullId (-32601)
                     in testCase "wrong method name capitalization" $ assertSubtractResponse req rsp

                   , let req = subtractRequestNamed [("X", Number 1), ("y", Number 20)] nonNullId
                         rsp = errResponse nonNullId (-32602)
                     in testCase "missing required named argument" $ assertSubtractResponse req rsp

                   , let req = TestRequest "subtract 2" (Just [Number 0]) (Just nonNullId)
                         rsp = errResponse nonNullId (-32602)
                     in testCase "missing required unnamed argument" $ assertSubtractResponse req rsp

                   , let req = subtractRequestNamed [("x", Number 1), ("y", String "2")] nonNullId
                         rsp = errResponse nonNullId (-32602)
                     in testCase "wrong argument type" $ assertSubtractResponse req rsp

                   , let req = subtractRequestUnnamed (map Number [1, 2, 3]) nonNullId
                         rsp = errResponse nonNullId (-32602)
                     in testCase "disallow extra unnamed arguments" $ assertSubtractResponse req rsp

                   , testCase "invalid notification" testNoResponseToInvalidNotification
                   , testCase "batch request" testBatch
                   , testCase "batch notifications" testBatchNotifications
                   , testCase "allow missing version" testAllowMissingVersion
                   , testCase "no arguments" testNoArgs
                   , testCase "empty argument array" testEmptyUnnamedArgs
                   , testCase "empty argument object" testEmptyNamedArgs
                   , testCase "allow extra named argument" testAllowExtraNamedArg
                   , testCase "use default named argument" testDefaultNamedArg
                   , testCase "use default unnamed argument" testDefaultUnnamedArg
                   , testCase "null request ID" testNullId
                   , testCase "parallelize tasks" P.testParallelizingTasks ]
                   
testEncodeRpcError :: Assertion
testEncodeRpcError = fromByteString (encode err) @?= Just testError
    where err = rpcError (-1) "error"
          testError = TestRpcError (-1) "error" Nothing

testEncodeErrorWithData :: Assertion
testEncodeErrorWithData = fromByteString (encode err) @?= Just testError
    where err = rpcErrorWithData 1 "my message" errorData
          testError = TestRpcError 1 "my message" $ Just $ toJSON errorData
          errorData = ('\x03BB', [True], ())

assertSubtractResponse :: ToJSON a => a -> TestResponse -> Assertion
assertSubtractResponse request expectedRsp = removeErrMsg <$> rsp @?= Just expectedRsp
    where rsp = callSubtractMethods request

testInvalidBatchElement :: Assertion
testInvalidBatchElement = map removeErrMsg <$> rsp @?= Just [errResponse idNull (-32600)]
      where rsp = callSubtractMethods [True]

testWrongVersion :: Assertion
testWrongVersion = removeErrMsg <$> rsp @?= Just (errResponse idNull (-32600))
    where rsp = callSubtractMethods $ Object $ H.insert versionKey (String "1") hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 4)] nonNullId

testNoResponseToInvalidNotification :: Assertion
testNoResponseToInvalidNotification = runIdentity response @?= Nothing
    where response = call (toMethods [subtractMethod]) $ encode request
          request = TestRequest "12345" (Nothing :: Maybe ()) Nothing

testBatch :: Assertion
testBatch = sortBy (compare `on` fromIntId) (fromJust (fromByteString =<< runIdentity response)) @?= expected
       where expected = [TestResponse i1 (Right $ Number 2), TestResponse i2 (Right $ Number 4)]
             response = call (toMethods [subtractMethod]) $ encode request
             request = [subtractRequestNamed (toArgs 10 8) i1, subtractRequestNamed (toArgs 24 20) i2]
             toArgs x y = [("x", Number x), ("y", Number y)]
             i1 = idNumber 1
             i2 = idNumber 2
             fromIntId rsp = (fromNumId $ rspId rsp) :: Maybe Int

testBatchNotifications :: Assertion
testBatchNotifications = runState response 0 @?= (Nothing, 10)
    where response = call (toMethods [incrementStateMethod]) $ encode request
          request = replicate 10 $ TestRequest "increment" (Nothing :: Maybe ()) Nothing

testAllowMissingVersion :: Assertion
testAllowMissingVersion = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 1))
    where requestNoVersion = Object $ H.delete versionKey hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 1)] i
          response = call (toMethods [subtractMethod]) $ encode requestNoVersion
          i = idNumber (-1)

testAllowExtraNamedArg :: Assertion
testAllowExtraNamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number (-10)))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args i
          args = [("x", Number 10), ("y", Number 20), ("z", String "extra")]
          i = idString "ID"

testDefaultNamedArg :: Assertion
testDefaultNamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 1000))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args i
          args = [("x1", Number 500), ("x", Number 1000)]
          i = idNumber 3

testDefaultUnnamedArg :: Assertion
testDefaultUnnamedArg = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse i (Right $ Number 4))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestUnnamed [Number 4] i
          i = idNumber 0

testNullId :: Assertion
testNullId = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse idNull (Right $ Number (-80)))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args idNull
          args = [("y", Number 70), ("x", Number (-10))]

testNoArgs :: Assertion
testNoArgs = compareGetTimeResult Nothing

testEmptyUnnamedArgs :: Assertion
testEmptyUnnamedArgs = compareGetTimeResult $ Just $ Right empty

testEmptyNamedArgs :: Assertion
testEmptyNamedArgs = compareGetTimeResult $ Just $ Left H.empty

incrementStateMethod :: Method (State Int)
incrementStateMethod = toMethod "increment" f ()
    where f :: RpcResult (State Int) ()
          f = lift $ modify (+1)

compareGetTimeResult :: Maybe (Either Object Array) -> Assertion
compareGetTimeResult requestArgs = assertEqual "unexpected rpc response" expected =<<
                                   ((fromByteString . fromJust) <$> call (toMethods [getTimeMethod]) (encode getTimeRequest))
    where expected = Just $ TestResponse i (Right $ Number 100)
          getTimeRequest = TestRequest "get_time_seconds" requestArgs (Just i)
          i = idString "Id 1"

subtractRequestNamed :: [(Text, Value)] -> TestId -> TestRequest
subtractRequestNamed args i = TestRequest "subtract 1" (Just $ H.fromList args) (Just i)

subtractRequestUnnamed :: [Value] -> TestId -> TestRequest
subtractRequestUnnamed args i = TestRequest "subtract 1" (Just args) (Just i)

callSubtractMethods :: (ToJSON a, FromJSON b) => a -> Maybe b
callSubtractMethods req = let methods :: Methods Identity
                              methods = toMethods [subtractMethod, flippedSubtractMethod]
                              rsp = call methods $ encode req
                          in fromByteString =<< runIdentity rsp

fromByteString :: FromJSON a => B.ByteString -> Maybe a
fromByteString str = case fromJSON <$> decode str of
                     Just (Success x) -> Just x
                     _ -> Nothing

subtractMethod :: Method Identity
subtractMethod = toMethod "subtract 1" sub (Required "x" :+: Optional "y" 0 :+: ())
            where sub :: Int -> Int -> RpcResult Identity Int
                  sub x y = return (x - y)

flippedSubtractMethod :: Method Identity
flippedSubtractMethod = toMethod "subtract 2" sub (Optional "y" (-1000) :+: Required "x" :+: ())
            where sub :: Int -> Int -> RpcResult Identity Int
                  sub y x = return (x - y)

getTimeMethod :: Method IO
getTimeMethod = toMethod "get_time_seconds" getTime ()
    where getTime :: RpcResult IO Integer
          getTime = liftIO getTestTime

getTestTime :: IO Integer
getTestTime = return 100

removeErrMsg :: TestResponse -> TestResponse
removeErrMsg (TestResponse i (Left (TestRpcError code _ _)))
            = TestResponse i (Left (TestRpcError code "" Nothing))
removeErrMsg rsp = rsp

errResponse :: TestId -> Int -> TestResponse
errResponse i code = TestResponse i (Left (TestRpcError code "" Nothing))

nonNullId :: TestId
nonNullId = idNumber 3

defaultArgs :: [Int]
defaultArgs = [1, 2]

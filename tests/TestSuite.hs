{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JsonRpc.Server
import TestTypes
import qualified TestParallelism as P
import Data.Maybe
import Data.List (sortBy)
import Data.Function (on)
import Data.Aeson as A
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
import Prelude hiding (subtract)

main :: IO ()
main = defaultMain [ testCase "encode RPC error" $
                         fromByteString (encode $ rpcError (-1) "error") @?= Just (TestRpcError (-1) "error" Nothing)

                   , let err = rpcErrorWithData 1 "my message" errData
                         testError = TestRpcError 1 "my message" $ Just $ toJSON errData
                         errData = ('\x03BB', [True], ())
                     in testCase "encode error with data" $ fromByteString (encode err) @?= Just testError

                   , testCase "invalid JSON" $
                         assertSubtractResponse ("5" :: String) (errResponse idNull (-32700))

                   , testCase "invalid JSON RPC" $
                         assertSubtractResponse (object ["id" .= (10 :: Int)]) (errResponse idNull (-32600))

                   , testCase "empty batch call" $
                         assertSubtractResponse emptyArray (errResponse idNull (-32600))

                   , testCase "invalid batch element" $
                         map removeErrMsg <$> callSubtractMethods [True] @?= Just [errResponse idNull (-32600)]

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

                   , let req = TestRequest "12345" (Nothing :: Maybe ()) Nothing
                     in testCase "invalid notification" $ callSubtractMethods req @?= (Nothing :: Maybe Value)

                   , testCase "batch request" testBatch
                   , testCase "batch notifications" testBatchNotifications
                   , testCase "allow missing version" testAllowMissingVersion

                   , testCase "no arguments" $
                         assertGetTimeResponse (Nothing :: Maybe Value)

                   , testCase "empty argument array" $
                         assertGetTimeResponse $ Just (empty :: Array)

                   , testCase "empty argument object" $
                         assertGetTimeResponse $ Just (H.empty :: Object)

                   , let req = subtractRequestNamed [("x", Number 10), ("y", Number 20), ("z", String "extra")] nonNullId
                         rsp = TestResponse nonNullId $ Right $ Number (-10)
                     in testCase "allow extra named argument" $ assertSubtractResponse req rsp

                   , let req = subtractRequestNamed [("x1", Number 500), ("x", Number 1000)] nonNullId
                         rsp = TestResponse nonNullId (Right $ Number 1000)
                     in testCase "use default named argument" $ assertSubtractResponse req rsp

                   , let req = subtractRequestUnnamed [Number 4] nonNullId
                         rsp = TestResponse nonNullId (Right $ Number 4)
                     in testCase "use default unnamed argument" $ assertSubtractResponse req rsp

                   , let req = subtractRequestNamed [("y", Number 70), ("x", Number (-10))] idNull
                         rsp = TestResponse idNull (Right $ Number (-80))
                     in testCase "null request ID" $ assertSubtractResponse req rsp

                   , testCase "parallelize tasks" P.testParallelizingTasks ]

assertSubtractResponse :: ToJSON a => a -> TestResponse -> Assertion
assertSubtractResponse request expectedRsp = removeErrMsg <$> rsp @?= Just expectedRsp
    where rsp = callSubtractMethods request

testWrongVersion :: Assertion
testWrongVersion = removeErrMsg <$> rsp @?= Just (errResponse idNull (-32600))
    where rsp = callSubtractMethods $ Object $ H.insert versionKey (String "1") hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 4)] nonNullId

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

incrementStateMethod :: Method (State Int)
incrementStateMethod = toMethod "increment" f ()
    where f :: RpcResult (State Int) ()
          f = lift $ modify (+1)

assertGetTimeResponse :: ToJSON a => a -> Assertion
assertGetTimeResponse args = passed @? "unexpected RPC response"
    where passed = (expected ==) <$> rsp
          expected = Just $ TestResponse nonNullId (Right $ Number 100)
          req = TestRequest "get_time_seconds" (Just args) (Just nonNullId)
          rsp = callGetTimeMethod req

subtractRequestNamed :: [(Text, Value)] -> TestId -> TestRequest
subtractRequestNamed args i = TestRequest "subtract" (Just $ H.fromList args) (Just i)

subtractRequestUnnamed :: [Value] -> TestId -> TestRequest
subtractRequestUnnamed args i = TestRequest "subtract" (Just args) (Just i)

callSubtractMethods :: (ToJSON a, FromJSON b) => a -> Maybe b
callSubtractMethods req = let methods :: Methods Identity
                              methods = toMethods [subtractMethod, flippedSubtractMethod]
                              rsp = call methods $ encode req
                          in fromByteString =<< runIdentity rsp

callGetTimeMethod :: TestRequest -> IO (Maybe TestResponse)
callGetTimeMethod req = let methods :: Methods IO
                            methods = toMethods [getTimeMethod]
                            rsp = call methods $ encode req
                        in (fromByteString =<<) <$> rsp

fromByteString :: FromJSON a => B.ByteString -> Maybe a
fromByteString str = case fromJSON <$> decode str of
                     Just (Success x) -> Just x
                     _ -> Nothing

subtractMethod :: Method Identity
subtractMethod = toMethod "subtract" subtract (Required "x" :+: Optional "y" 0 :+: ())

flippedSubtractMethod :: Method Identity
flippedSubtractMethod = toMethod "subtract 2" (flip subtract) params
    where params = Optional "y" (-1000) :+: Required "x" :+: ()

subtract :: Int -> Int -> RpcResult Identity Int
subtract x y = return (x - y)

getTimeMethod :: Method IO
getTimeMethod = toMethod "get_time_seconds" getTestTime ()
    where getTestTime :: RpcResult IO Integer
          getTestTime = liftIO $ return 100

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

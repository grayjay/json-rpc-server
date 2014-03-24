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
import Test.HUnit hiding (State, Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import Prelude hiding (subtract)

main :: IO ()
main = defaultMain $ errorHandlingTests ++ otherTests

errorHandlingTests :: [Test]
errorHandlingTests = [ testCase "invalid JSON" $
                           assertSubtractResponse ("5" :: String) (errResponse idNull (-32700))

                     , testCase "invalid JSON RPC" $
                           assertSubtractResponse (object ["id" .= (10 :: Int)]) (errResponse idNull (-32600))

                     , testCase "empty batch call" $
                           assertSubtractResponse emptyArray (errResponse idNull (-32600))

                     , testCase "invalid batch element" $
                           map removeErrMsg <$> callSubtractMethods [True] @?= Just [errResponse idNull (-32600)]

                     , testCase "wrong request version" testWrongVersion

                     , testCase "wrong id type" testWrongIdType

                     , let req = TestRequest "add" (Just defaultArgs) (Just defaultIdNonNull)
                           rsp = errResponse defaultIdNonNull (-32601)
                       in testCase "method not found" $ assertSubtractResponse req rsp

                     , let req = TestRequest "Subtract" (Just defaultArgs) (Just defaultIdNonNull)
                           rsp = errResponse defaultIdNonNull (-32601)
                       in testCase "wrong method name capitalization" $ assertSubtractResponse req rsp

                     , testCase "missing required named argument" $
                           assertInvalidParams "subtract" $ object ["a" .= Number 1, "y" .= Number 20]

                     , testCase "missing required unnamed argument" $
                           assertInvalidParams "subtract 2" [Number 0]

                     , testCase "wrong argument type" $
                           assertInvalidParams "subtract" $ object [("x", Number 1), ("y", String "2")]

                     , testCase "extra unnamed arguments" $
                           assertInvalidParams "subtract" $ map Number [1, 2, 3]

                     , let req = TestRequest "12345" (Just defaultArgs) Nothing
                       in testCase "invalid notification" $ callSubtractMethods req @?= (Nothing :: Maybe Value) ]

otherTests :: [Test]
otherTests = [ testCase "encode RPC error" $
                   fromByteString (encode $ rpcError (-1) "error") @?= Just (TestRpcError (-1) "error" Nothing)

             , let err = rpcErrorWithData 1 "my message" errData
                   testError = TestRpcError 1 "my message" $ Just $ toJSON errData
                   errData = ('\x03BB', [True], ())
               in testCase "encode RPC error with data" $ fromByteString (encode err) @?= Just testError

             , testCase "batch request" testBatch
             , testCase "batch notifications" testBatchNotifications
             , testCase "allow missing version" testAllowMissingVersion

             , testCase "no arguments" $
                   assertGetTimeResponse (Nothing :: Maybe Value)

             , testCase "empty argument array" $
                   assertGetTimeResponse $ Just (empty :: Array)

             , testCase "empty argument object" $
                   assertGetTimeResponse $ Just (H.empty :: Object)

             , let req = subtractRequestNamed ["x" .= Number 10, "y" .= Number 20, "z" .= String "extra"]
                   rsp = TestResponse defaultIdNonNull $ Right $ Number (-10)
               in testCase "allow extra named argument" $ assertSubtractResponse req rsp

             , let req = subtractRequestNamed [("x1", Number 500), ("x", Number 1000)]
                   rsp = TestResponse defaultIdNonNull (Right $ Number 1000)
               in testCase "use default named argument" $ assertSubtractResponse req rsp

             , let req = subtractRequestUnnamed [Number 4]
                   rsp = TestResponse defaultIdNonNull (Right $ Number 4)
               in testCase "use default unnamed argument" $ assertSubtractResponse req rsp

             , testCase "string request ID" $ assertEqualId $ idString "ID 5"

             , testCase "null request ID" $ assertEqualId idNull

             , testCase "parallelize tasks" P.testParallelizingTasks ]

assertSubtractResponse :: ToJSON a => a -> TestResponse -> Assertion
assertSubtractResponse request expectedRsp = removeErrMsg <$> rsp @?= Just expectedRsp
    where rsp = callSubtractMethods request

assertEqualId :: TestId -> Assertion
assertEqualId i = let req = TestRequest "subtract" (Just defaultArgs) (Just i)
                      rsp = TestResponse i $ Right defaultResult
                  in assertSubtractResponse req rsp

assertInvalidParams :: ToJSON a => Text -> a -> Assertion
assertInvalidParams name args = let req = TestRequest name (Just args) $ Just defaultIdNonNull
                                    rsp = errResponse defaultIdNonNull (-32602)
                                in assertSubtractResponse req rsp

testWrongVersion :: Assertion
testWrongVersion = removeErrMsg <$> rsp @?= Just (errResponse idNull (-32600))
    where rsp = callSubtractMethods $ Object $ H.insert versionKey (String "1") hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 4)]

testWrongIdType :: Assertion
testWrongIdType = removeErrMsg <$> rsp @?= Just (errResponse idNull (-32600))
    where rsp = callSubtractMethods $ Object $ H.insert idKey (Bool True) hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 4)]

testBatch :: Assertion
testBatch = sortBy (compare `on` fromIntId) (fromJust (fromByteString =<< runIdentity response)) @?= expected
       where expected = [TestResponse i1 (Right $ Number 2), TestResponse i2 (Right $ Number 4)]
             response = call (toMethods [subtractMethod]) $ encode request
             request = [TestRequest "subtract" (toArgs 10 8) (Just i1), TestRequest "subtract" (toArgs 24 20) (Just i2)]
             toArgs :: Int -> Int -> Maybe Value
             toArgs x y = Just $ object ["x" .= x, "y" .= y]
             i1 = idNumber 1
             i2 = idNumber 2
             fromIntId rsp = (fromNumId $ rspId rsp) :: Maybe Int

testBatchNotifications :: Assertion
testBatchNotifications = runState response 0 @?= (Nothing, 10)
    where response = call (toMethods [incrementStateMethod]) $ encode request
          request = replicate 10 $ TestRequest "increment" (Nothing :: Maybe ()) Nothing

testAllowMissingVersion :: Assertion
testAllowMissingVersion = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse defaultIdNonNull (Right $ Number 1))
    where requestNoVersion = Object $ H.delete versionKey hm
          Object hm = toJSON $ subtractRequestNamed [("x", Number 1)]
          response = call (toMethods [subtractMethod]) $ encode requestNoVersion

incrementStateMethod :: Method (State Int)
incrementStateMethod = toMethod "increment" f ()
    where f :: RpcResult (State Int) ()
          f = lift $ modify (+1)

assertGetTimeResponse :: ToJSON a => a -> Assertion
assertGetTimeResponse args = passed @? "unexpected RPC response"
    where passed = (expected ==) <$> rsp
          expected = Just $ TestResponse defaultIdNonNull (Right $ Number 100)
          req = TestRequest "get_time_seconds" (Just args) (Just defaultIdNonNull)
          rsp = callGetTimeMethod req

subtractRequestNamed :: [(Text, Value)] -> TestRequest
subtractRequestNamed args = TestRequest "subtract" (Just $ H.fromList args) (Just defaultIdNonNull)

subtractRequestUnnamed :: [Value] -> TestRequest
subtractRequestUnnamed args = TestRequest "subtract" (Just args) (Just defaultIdNonNull)

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

defaultIdNonNull :: TestId
defaultIdNonNull = idNumber 3

defaultArgs :: [Int]
defaultArgs = [1, 2]

defaultResult :: Value
defaultResult = Number (-1)

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JsonRpc.Server
import Internal
import qualified TestParallelism
import Data.Maybe
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
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
                           assertSubtractResponse (A.String "5") (errRsp A.Null (-32700))

                     , testCase "invalid JSON RPC" $
                           assertSubtractResponse (A.object ["id" .= A.Number 10]) (errRsp A.Null (-32600))

                     , testCase "empty batch call" $
                           assertSubtractResponse A.emptyArray (errRsp A.Null (-32600))

                     , testCase "invalid batch element" $
                           removeErrMsg <$> callSubtractMethods (array [A.Bool True]) @?= Just (array [errRsp A.Null (-32600)])

                     , testCase "wrong request version" testWrongVersion

                     , testCase "wrong id type" testWrongIdType

                     , let req = idRequest "add" $ Just defaultArgs
                           rsp = idErrRsp (-32601)
                       in testCase "method not found" $ assertSubtractResponse req rsp

                     , let req = idRequest "Subtract" $ Just defaultArgs
                           rsp = idErrRsp (-32601)
                       in testCase "wrong method name capitalization" $ assertSubtractResponse req rsp

                     , testCase "missing required named argument" $
                           assertInvalidParams "subtract" $ A.object ["a" .= A.Number 1, "y" .= A.Number 20]

                     , testCase "missing required unnamed argument" $
                           assertInvalidParams "subtract 2" $ array [A.Number 0]

                     , testCase "wrong argument type" $
                           assertInvalidParams "subtract" $ A.object [("x", A.Number 1), ("y", A.String "2")]

                     , testCase "extra unnamed arguments" $
                           assertInvalidParams "subtract" $ array $ map A.Number [1, 2, 3]

                     , let req = request2_0 Nothing "12345" $ Just defaultArgs
                       in testCase "invalid notification" $ callSubtractMethods req @?= (Nothing :: Maybe A.Value) ]

otherTests :: [Test]
otherTests = [ testCase "encode RPC error" $
                   A.toJSON (rpcError (-1) "error") @?= rpcErr Nothing (-1) "error"

             , let err = rpcErrorWithData 1 "my message" errData
                   testError = rpcErr (Just $ A.toJSON errData) 1 "my message"
                   errData = ('\x03BB', [True], ())
               in testCase "encode RPC error with data" $ A.toJSON err @?= testError

             , testCase "batch request" testBatch
             , testCase "batch notifications" testBatchNotifications
             , testCase "allow missing version" testAllowMissingVersion

             , testCase "no arguments" $
                   assertGetTimeResponse Nothing

             , testCase "empty argument array" $
                   assertGetTimeResponse $ Just A.emptyArray

             , testCase "empty argument A.object" $
                   assertGetTimeResponse $ Just A.emptyObject

             , let req = subtractRq $ Just $ A.object ["x" .= A.Number 10, "y" .= A.Number 20, "z" .= A.String "extra"]
                   rsp = idSuccessRsp $ A.Number (-10)
               in testCase "allow extra named argument" $ assertSubtractResponse req rsp

             , let req = subtractRq $ Just $ A.object [("x1", A.Number 500), ("x", A.Number 1000)]
                   rsp = idSuccessRsp $ A.Number 1000
               in testCase "use default named argument" $ assertSubtractResponse req rsp

             , let req = subtractRq $ Just $ array [A.Number 4]
                   rsp = idSuccessRsp $ A.Number 4
               in testCase "use default unnamed argument" $ assertSubtractResponse req rsp

             , testCase "string request ID" $ assertEqualId $ A.String "ID 5"

             , testCase "null request ID" $ assertEqualId A.Null

             , testCase "parallelize tasks" TestParallelism.testParallelizingTasks ]

assertSubtractResponse :: A.Value -> A.Value -> Assertion
assertSubtractResponse rq expectedRsp = removeErrMsg <$> rsp @?= Just expectedRsp
    where rsp = callSubtractMethods rq

assertEqualId :: A.Value -> Assertion
assertEqualId i = let req = request2_0 (Just i) "subtract" (Just defaultArgs)
                      rsp = successRsp i defaultResult
                  in assertSubtractResponse req rsp

assertInvalidParams :: Text -> A.Value -> Assertion
assertInvalidParams name args = let req = idRequest name $ Just args
                                    rsp = idErrRsp (-32602)
                                in assertSubtractResponse req rsp

testWrongVersion :: Assertion
testWrongVersion = removeErrMsg <$> rsp @?= Just (errRsp A.Null (-32600))
    where rsp = callSubtractMethods $ request ver (Just defaultId) (A.String "subtract") args
          ver = Just "1.0"
          args = Just $ A.object ["x" .= A.Number 4]

testWrongIdType :: Assertion
testWrongIdType = removeErrMsg <$> rsp @?= Just (errRsp A.Null (-32600))
    where rsp = callSubtractMethods $ request2_0 (Just $ A.Bool True) "subtract" args
          args = Just $ A.object ["x" .= A.Number 4]

testBatch :: Assertion
testBatch = sortBy (compare `on` idToString) <$> response @?= Just expected
       where expected = [successRsp i1 (A.Number 2), successRsp i2 (A.Number 4)]
             response :: Maybe [A.Value]
             response = A.decode =<< runIdentity (call (toMethods [subtractMethod]) $ A.encode rq)
             rq = [request2_0 (Just i1) "subtract" (toArgs 10 8), request2_0 (Just i2) "subtract" (toArgs 24 20)]
             toArgs :: Int -> Int -> Maybe A.Value
             toArgs x y = Just $ A.object ["x" .= x, "y" .= y]
             i1 = A.Number 1
             i2 = A.Number 2
             idToString :: A.Value -> Maybe String
             idToString (A.Object rsp) = show <$> H.lookup idKey rsp
             idToString _ = Nothing

testBatchNotifications :: Assertion
testBatchNotifications = runState response 0 @?= (Nothing, 10)
    where response = call (toMethods [incrementStateMethod]) $ A.encode rq
          rq = replicate 10 $ request2_0 Nothing "increment" Nothing

testAllowMissingVersion :: Assertion
testAllowMissingVersion = (fromByteString =<< runIdentity response) @?= (Just $ idSuccessRsp (A.Number 1))
    where response = call (toMethods [subtractMethod]) $ A.encode requestNoVersion
          requestNoVersion = request Nothing (Just defaultId) "subtract" args
          args = Just $ A.object ["x" .= A.Number 1]

incrementStateMethod :: Method (State Int)
incrementStateMethod = toMethod "increment" f ()
    where f :: RpcResult (State Int) ()
          f = lift $ modify (+1)

assertGetTimeResponse :: Maybe A.Value -> Assertion
assertGetTimeResponse args = passed @? "unexpected RPC response"
    where passed = (expected ==) <$> rsp
          expected = Just $ idSuccessRsp (A.Number 100)
          req = idRequest "get_time_seconds" args
          rsp = callGetTimeMethod req

callSubtractMethods :: A.Value -> Maybe A.Value
callSubtractMethods req = let methods :: Methods Identity
                              methods = toMethods [subtractMethod, flippedSubtractMethod]
                              rsp = call methods $ A.encode req
                          in fromByteString =<< runIdentity rsp

callGetTimeMethod :: A.Value -> IO (Maybe A.Value)
callGetTimeMethod req = let methods :: Methods IO
                            methods = toMethods [getTimeMethod]
                            rsp = call methods $ A.encode req
                        in (fromByteString =<<) <$> rsp

fromByteString :: A.FromJSON a => B.ByteString -> Maybe a
fromByteString str = case A.fromJSON <$> A.decode str of
                     Just (A.Success x) -> Just x
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

removeErrMsg :: A.Value -> A.Value
removeErrMsg (A.Object rsp) = A.Object $ H.adjust removeMsg errKey rsp
    where removeMsg (A.Object err) = A.Object $ H.insert msgKey "" $ H.delete dataKey err
          removeMsg v = v
removeErrMsg (A.Array rsps) = A.Array $ removeErrMsg <$> rsps
removeErrMsg v = v

defaultArgs :: A.Value
defaultArgs = array $ map A.Number [1, 2]

defaultResult :: A.Value
defaultResult = A.Number (-1)

subtractRq :: Maybe A.Value -> A.Value
subtractRq = idRequest "subtract"

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Network.JsonRpc.Server as S
import Network.JsonRpc.Server ((:+:) (..))
import Internal ( request, defaultRq, defaultRsp
                , defaultIdErrRsp, nullIdErrRsp
                , version, result, rpcErr, method
                , params, id', array, rspToIdString)
import qualified TestParallelism
import Data.List (sortBy)
import qualified Data.Vector as V
import Data.Function (on)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Control.Monad.State (State, runState, lift, modify)
import Control.Monad.Identity (Identity, runIdentity)
import Test.HUnit hiding (State, Test)
import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase)
import Prelude hiding (subtract)

main :: IO ()
main = defaultMain $ errorHandlingTests ++ otherTests

errorHandlingTests :: [Test]
errorHandlingTests = [ testCase "invalid JSON" $
                           assertSubtractResponse (A.String "5") $ nullIdErrRsp (-32700)

                     , testCase "invalid JSON RPC" $
                           assertSubtractResponse (A.object ["id" .= A.Number 10]) $ nullIdErrRsp (-32600)

                     , testCase "empty batch call" $
                           assertSubtractResponse A.emptyArray $ nullIdErrRsp (-32600)

                     , testCase "invalid batch element" $
                           removeErrMsg <$> callSubtractMethods (array [A.Bool True]) @?= Just (array [nullIdErrRsp (-32600)])

                     , testCase "wrong request version" $
                           assertSubtractResponse (defaultRq `version` Just "1.0") $ nullIdErrRsp (-32600)

                     , testCase "wrong id type" $
                           assertSubtractResponse (defaultRq `id'` (Just $ A.Bool True)) $ nullIdErrRsp (-32600)

                     , testCase "method not found" $
                           assertSubtractResponse (defaultRq `method` "add") (defaultIdErrRsp (-32601))

                     , testCase "wrong method name capitalization" $
                           assertSubtractResponse (defaultRq `method` "Subtract") (defaultIdErrRsp (-32601))

                     , testCase "missing required named argument" $
                           assertInvalidParams $ defaultRq `params` Just (A.object ["a" .= A.Number 1, "y" .= A.Number 20])

                     , testCase "missing required unnamed argument" $
                           assertInvalidParams $ defaultRq `method` "flipped subtract" `params` Just (array [A.Number 0])

                     , testCase "wrong argument type" $
                           assertInvalidParams $ defaultRq `params` Just (A.object ["x" .= A.Number 1, "y" .= A.String "2"])

                     , testCase "extra unnamed arguments" $
                           assertInvalidParams $ defaultRq `params` Just (array $ map A.Number [1, 2, 3])

                     , let req = defaultRq `id'` Nothing `method` "12345"
                       in testCase "invalid notification" $ callSubtractMethods req @?= Nothing ]

otherTests :: [Test]
otherTests = [ testCase "encode RPC error" $
                   A.toJSON (S.rpcError (-1) "error") @?= rpcErr Nothing (-1) "error"

             , let err = S.rpcErrorWithData 1 "my message" errData
                   testError = rpcErr (Just $ A.toJSON errData) 1 "my message"
                   errData = ('\x03BB', [True], ())
               in testCase "encode RPC error with data" $ A.toJSON err @?= testError

             , testCase "batch request" testBatch

             , testCase "batch notifications" testBatchNotifications

             , testCase "allow missing version" testAllowMissingVersion

             , testCase "no arguments" $ assertGetTimeResponse Nothing

             , testCase "empty argument array" $ assertGetTimeResponse $ Just A.emptyArray

             , testCase "empty argument A.object" $ assertGetTimeResponse $ Just A.emptyObject

             , let req = defaultRq `params` Just args
                   args = A.object ["x" .= A.Number 10, "y" .= A.Number 20, "z" .= A.String "extra"]
                   rsp = defaultRsp `result` A.Number (-10)
               in testCase "allow extra named argument" $ assertSubtractResponse req rsp

             , let req = defaultRq `params` (Just $ A.object [("x1", A.Number 500), ("x", A.Number 1000)])
                   rsp = defaultRsp `result` A.Number 1000
               in testCase "use default named argument" $ assertSubtractResponse req rsp

             , let req = defaultRq `params` (Just $ array [A.Number 4])
                   rsp = defaultRsp `result` A.Number 4
               in testCase "use default unnamed argument" $ assertSubtractResponse req rsp

             , testCase "string request ID" $ assertEqualId $ A.String "ID 5"

             , testCase "null request ID" $ assertEqualId A.Null

             , testCase "parallelize tasks" TestParallelism.testParallelizingTasks ]

assertSubtractResponse :: A.Value -> A.Value -> Assertion
assertSubtractResponse rq expectedRsp = removeErrMsg <$> rsp @?= Just expectedRsp
    where rsp = callSubtractMethods rq

assertEqualId :: A.Value -> Assertion
assertEqualId i = assertSubtractResponse (defaultRq `id'` Just i) (defaultRsp `id'` Just i)

assertInvalidParams :: A.Value -> Assertion
assertInvalidParams req = assertSubtractResponse req (defaultIdErrRsp (-32602))

testBatch :: Assertion
testBatch = sortBy (compare `on` rspToIdString) <$> response @?= Just expected
       where expected = [nullIdErrRsp (-32600), rsp i1 2, rsp i2 4] 
                 where rsp i x = defaultRsp `id'` Just i `result` A.Number x
             response = fromArray =<< (removeErrMsg <$> callSubtractMethods (array requests))
             requests = [rq (Just i1) 10 8, rq (Just i2) 24 20, rq Nothing 15 1, defaultRq `version` Just (A.String "abc")]
                 where rq i x y = defaultRq `id'` i `params` toArgs x y
             toArgs :: Int -> Int -> Maybe A.Value
             toArgs x y = Just $ A.object ["x" .= x, "y" .= y]
             i1 = A.Number 1
             i2 = A.Number 2
             fromArray (A.Array v) = Just $ V.toList v
             fromArray _ = Nothing

testBatchNotifications :: Assertion
testBatchNotifications = runState response 0 @?= (Nothing, 10)
    where response = S.call (S.toMethods [incrementStateMethod]) $ A.encode rq
          rq = replicate 10 $ request Nothing "increment" Nothing

testAllowMissingVersion :: Assertion
testAllowMissingVersion = callSubtractMethods requestNoVersion @?= (Just $ defaultRsp `result` A.Number 1)
    where requestNoVersion = defaultRq `version` Nothing `params` Just (A.object ["x" .= A.Number 1])

incrementStateMethod :: S.Method (State Int)
incrementStateMethod = S.toMethod "increment" f ()
    where f :: S.RpcResult (State Int) ()
          f = lift $ modify (+1)

assertGetTimeResponse :: Maybe A.Value -> Assertion
assertGetTimeResponse args = passed @? "unexpected RPC response"
    where passed = (expected ==) <$> rsp
          expected = Just $ defaultRsp `result` A.Number 100
          req = defaultRq `method` "get_time_seconds" `params` args
          rsp = callGetTimeMethod req

callSubtractMethods :: A.Value -> Maybe A.Value
callSubtractMethods req = let methods :: S.Methods Identity
                              methods = S.toMethods [subtractMethod, flippedSubtractMethod]
                              rsp = S.call methods $ A.encode req
                          in A.decode =<< runIdentity rsp

callGetTimeMethod :: A.Value -> IO (Maybe A.Value)
callGetTimeMethod req = let methods :: S.Methods IO
                            methods = S.toMethods [getTimeMethod]
                            rsp = S.call methods $ A.encode req
                        in (A.decode =<<) <$> rsp

subtractMethod :: S.Method Identity
subtractMethod = S.toMethod "subtract" subtract (S.Required "x" :+: S.Optional "y" 0 :+: ())

flippedSubtractMethod :: S.Method Identity
flippedSubtractMethod = S.toMethod "flipped subtract" (flip subtract) ps
    where ps = S.Optional "y" (-1000) :+: S.Required "x" :+: ()

subtract :: Int -> Int -> S.RpcResult Identity Int
subtract x y = return (x - y)

getTimeMethod :: S.Method IO
getTimeMethod = S.toMethod "get_time_seconds" getTestTime ()
    where getTestTime :: S.RpcResult IO Integer
          getTestTime = liftIO $ return 100

removeErrMsg :: A.Value -> A.Value
removeErrMsg (A.Object rsp) = A.Object $ H.adjust removeMsg "error" rsp
    where removeMsg (A.Object err) = A.Object $ H.insert "message" "" $ H.delete "data" err
          removeMsg v = v
removeErrMsg (A.Array rsps) = A.Array $ removeErrMsg `V.map` rsps
removeErrMsg v = v

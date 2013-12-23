{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Main (main) where

import Network.JsonRpc.Server
import TestTypes
import Data.List ((\\), sort)
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Identity
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Test.HUnit hiding (State)
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testCase "encode error" testEncodeError
                   , testCase "encode error with data" testEncodeErrorWithData
                   , testCase "invalid JSON" testInvalidJson
                   , testCase "invalid JSON RPC" testInvalidJsonRpc
                   , testCase "empty batch call" testEmptyBatchCall
                   , testCase "wrong version in request" testWrongVersion
                   , testCase "method not found" testMethodNotFound
                   , testCase "wrong method name capitalization" testWrongMethodNameCapitalization
                   , testCase "missing required named argument" testMissingRequiredNamedArg
                   , testCase "missing required unnamed argument" testMissingRequiredUnnamedArg
                   , testCase "wrong argument type" testWrongArgType
                   , testCase "disallow extra unnamed arguments" testDisallowExtraUnnamedArg
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
                   , testCase "parallelize tasks" testParallelizingTasks ]

testEncodeError :: Assertion
testEncodeError = (fromByteString $ encode $ toJSON err) @?= Just testError
    where err = rpcError (-1) "error"
          testError = TestRpcError (-1) "error" Nothing

testEncodeErrorWithData :: Assertion
testEncodeErrorWithData = (fromByteString $ encode $ toJSON err) @?= Just testError
    where err = rpcErrorWithData 1 "my message" errorData
          testError = TestRpcError 1 "my message" $ Just $ toJSON errorData
          errorData = (['\x03BB'], True, ())

testInvalidJson :: Assertion
testInvalidJson = checkResponseWithSubtract "5" IdNull (-32700)

testInvalidJsonRpc :: Assertion
testInvalidJsonRpc = checkResponseWithSubtract (encode $ object ["id" .= (10 :: Int)]) IdNull (-32600)

testEmptyBatchCall :: Assertion
testEmptyBatchCall = checkResponseWithSubtract (encode emptyArray) IdNull (-32600)

testWrongVersion :: Assertion
testWrongVersion = checkResponseWithSubtract (encode requestWrongVersion) IdNull (-32600)
    where requestWrongVersion = Object $ H.insert versionKey (String "1") hm
          Object hm = toJSON $ subtractRequestNamed [("a1", Number 4)] (IdNumber 10)

testMethodNotFound :: Assertion
testMethodNotFound = checkResponseWithSubtract (encode request) i (-32601)
    where request = TestRequest "ad" Nothing (Just i)
          i = IdNumber 3

testWrongMethodNameCapitalization :: Assertion
testWrongMethodNameCapitalization = checkResponseWithSubtract (encode request) i (-32601)
    where request = TestRequest "Add" Nothing (Just i)
          i = IdNull

testMissingRequiredNamedArg :: Assertion
testMissingRequiredNamedArg = checkResponseWithSubtract (encode request) i (-32602)
    where request = subtractRequestNamed [("A1", Number 1), ("a2", Number 20)] i
          i = IdNumber 2

testMissingRequiredUnnamedArg :: Assertion
testMissingRequiredUnnamedArg = checkResponseWithSubtract (encode request) i (-32602)
    where request = TestRequest "subtract 2" (Just $ Right $ V.fromList [Number 0]) (Just i)
          i = IdString ""

testWrongArgType :: Assertion
testWrongArgType = checkResponseWithSubtract (encode request) i (-32602)
    where request = subtractRequestNamed [("a1", Number 1), ("a2", Bool True)] i
          i = IdString "ABC"

testDisallowExtraUnnamedArg :: Assertion
testDisallowExtraUnnamedArg = checkResponseWithSubtract (encode request) i (-32602)
    where request = subtractRequestUnnamed (map Number [1, 2, 3]) i
          i = IdString "i"

testNoResponseToInvalidNotification :: Assertion
testNoResponseToInvalidNotification = runIdentity response @?= Nothing
    where response = call (toMethods [subtractMethod]) $ encode request
          request = TestRequest "12345" Nothing Nothing

testBatch :: Assertion
testBatch = assert (fromJust (fromByteString =<< runIdentity response) `equalContents` expected)
    where expected = [TestResponse i1 (Right $ Number 2), TestResponse i2 (Right $ Number 4)]
          response = call (toMethods [subtractMethod]) $ encode request
          request = [subtractRequestNamed (toArgs 10 8) i1, subtractRequestNamed (toArgs 24 20) i2]
          toArgs x y = [("a1", Number x), ("a2", Number y)]
          i1 = IdString "1"
          i2 = IdString "2"

testBatchNotifications :: Assertion
testBatchNotifications = runState response 0 @?= (Nothing, 10)
    where response = call (toMethods [incrementStateMethod]) $ encode request
          request = replicate 10 $ TestRequest "increment" Nothing Nothing

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
testNullId = (fromByteString =<< runIdentity response) @?= (Just $ TestResponse IdNull (Right $ Number (-80)))
    where response = call (toMethods [subtractMethod]) $ encode request
          request = subtractRequestNamed args IdNull
          args = [("a2", Number 70), ("a1", Number (-10))]

testNoArgs :: Assertion
testNoArgs = compareGetTimeResult Nothing

testEmptyUnnamedArgs :: Assertion
testEmptyUnnamedArgs = compareGetTimeResult $ Just $ Right empty

testEmptyNamedArgs :: Assertion
testEmptyNamedArgs = compareGetTimeResult $ Just $ Left H.empty

testParallelizingTasks :: Assertion
testParallelizingTasks = assert $ do
                           a <- actual
                           let ids = map fromIdNumber a
                               vals = map fromResult a
                           return $ (sort ids == [1, 2]) &&
                                    (sort vals == ["A", "B"])
    where actual = (fromJust . fromByteString . fromJust) <$> (flip (callWithBatchStrategy parallelize) input =<< methods)
          input = encode [ lockRequest 1, unlockRequest (String "A")
                         , unlockRequest (String "B"), lockRequest 2]
          lockRequest i = TestRequest "lock" Nothing (Just $ IdNumber i)
          unlockRequest str = TestRequest "unlock" (Just $ Right $ V.fromList [str]) Nothing
          methods = createMethods <$> newEmptyMVar
          createMethods lock = toMethods [lockMethod lock, unlockMethod lock]
          lockMethod lock = toMethod "lock" f ()
              where f :: RpcResult IO String
                    f = liftIO $ takeMVar lock
          unlockMethod lock = toMethod "unlock" f (Required "value" :+: ())
              where f :: String -> RpcResult IO ()
                    f val = liftIO $ putMVar lock val
          fromResult r | Right (String str) <- rspResult r = str
          fromIdNumber r | IdNumber i <- rspId r = i

parallelize :: [IO a] -> IO [a]
parallelize tasks = do
  results <- forM tasks $ \t -> do
                      mvar <- newEmptyMVar
                      _ <- forkIO $ putMVar mvar =<< t
                      return mvar
  forM results takeMVar

incrementStateMethod :: Method (State Int)
incrementStateMethod = toMethod "increment" f ()
    where f :: RpcResult (State Int) ()
          f = lift $ modify (+1)

compareGetTimeResult :: Maybe (Either Object Array) -> Assertion
compareGetTimeResult requestArgs = assertEqual "unexpected rpc response" expected =<<
                                   ((fromByteString . fromJust) <$> call (toMethods [getTimeMethod]) (encode getTimeRequest))
    where expected = Just $ TestResponse i (Right $ Number 100)
          getTimeRequest = TestRequest "get_time_seconds" requestArgs (Just i)
          i = IdString "Id 1"

subtractRequestNamed :: [(Text, Value)] -> TestId -> TestRequest
subtractRequestNamed args i = TestRequest "subtract 1" (Just $ Left $ H.fromList args) (Just i)

subtractRequestUnnamed :: [Value] -> TestId -> TestRequest
subtractRequestUnnamed args i = TestRequest "subtract 1" (Just $ Right $ V.fromList args) (Just i)

checkResponseWithSubtract :: B.ByteString -> TestId -> Int -> Assertion
checkResponseWithSubtract val expectedId expectedCode = actual @?= expected
    where expected = (Just expectedId, Just expectedCode)
          actual = (rspId <$> fromByteString result, getErrorCode result)
          result = fromJust $ runIdentity $ call (toMethods [subtractMethod, flippedSubtractMethod]) val

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
subtractMethod = toMethod "subtract 1" sub (Required "a1" :+: Optional "a2" 0 :+: ())
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

equalContents :: Eq a => [a] -> [a] -> Bool
equalContents xs ys = null (xs \\ ys) &&
                      null (ys \\ xs)

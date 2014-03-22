{-# LANGUAGE OverloadedStrings #-}

module TestParallelism (testParallelizingTasks) where

import Network.JsonRpc.Server
import TestTypes
import Data.List (sort)
import qualified Data.Aeson as A
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Test.HUnit ((@?=), Assertion)

-- | Tests sending a batch request where each request either
--   locks or unlocks an MVar.  The MVar is initially locked,
--   so the first lock request would not succeed if the
--   requests were serialized.
testParallelizingTasks :: Assertion
testParallelizingTasks = do
  methods <- createMethods <$> newEmptyMVar
  output <- callWithBatchStrategy parallelize methods input
  let result = fromJust $ fromByteString =<< output
  sort (map rspToIntId result) @?= map Just [1..3] 
  sort (map rspToCharVal result) @?= map Just "ABC"
    where input = A.encode [ lockRequest 1
                           , lockRequest 3
                           , unlockRequest 'C'
                           , unlockRequest 'B'
                           , lockRequest 2
                           , unlockRequest 'A']
          createMethods lock = toMethods [lockMethod lock, unlockMethod lock]

rspToIntId :: TestResponse -> Maybe Int
rspToIntId = fromNumId . rspId

rspToCharVal :: TestResponse -> Maybe Char
rspToCharVal resp = let (Right r) = rspResult resp
                    in fromJust $ fromJson r

lockRequest :: Int -> TestRequest
lockRequest i = TestRequest "lock" (Nothing :: Maybe ()) (Just $ idNumber i)

unlockRequest :: Char -> TestRequest
unlockRequest ch = TestRequest "unlock" (Just $ H.fromList [("value" :: String, ch)]) Nothing

lockMethod :: MVar Char -> Method IO
lockMethod lock = toMethod "lock" f ()
    where f :: RpcResult IO Char
          f = liftIO $ takeMVar lock

unlockMethod :: MVar Char -> Method IO
unlockMethod lock = toMethod "unlock" f (Required "value" :+: ())
    where f :: Char -> RpcResult IO ()
          f val = liftIO $ putMVar lock val

parallelize :: [IO a] -> IO [a]
parallelize tasks = mapM takeMVar =<< mapM fork tasks
      where fork t = do
                      mvar <- newEmptyMVar
                      _ <- forkIO $ putMVar mvar =<< t
                      return mvar

fromByteString :: A.FromJSON a => B.ByteString -> Maybe a
fromByteString str = case A.fromJSON <$> A.decode str of
                     Just (A.Success x) -> Just x
                     _ -> Nothing

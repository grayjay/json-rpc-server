{-# LANGUAGE OverloadedStrings #-}

module TestParallelism (testParallelizingTasks) where

import Network.JsonRpc.Server
import Internal
import Data.List (sort)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as H
import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Test.HUnit ((@?=), Assertion)

-- | Tests parallelizing a batch request.  Each request either
--   locks or unlocks an MVar.  The MVar is initially locked,
--   so the first lock request cannot succeed if the requests
--   are serialized.
testParallelizingTasks :: Assertion
testParallelizingTasks = do
  methods <- createMethods <$> newEmptyMVar
  output <- callWithBatchStrategy parallelize methods input
  let rsp = fromJust $ fromJson =<< A.decode =<< output
  sort (map rspToIntId rsp) @?= map Just [1..3]
  sort (map rspToCharResult rsp) @?= map Just "ABC"
    where input = A.encode [ lockRequest 1
                           , lockRequest 3
                           , unlockRequest 'C'
                           , unlockRequest 'B'
                           , lockRequest 2
                           , unlockRequest 'A']
          createMethods lock = toMethods [lockMethod lock, unlockMethod lock]

rspToIntId :: A.Value -> Maybe Int
rspToIntId (A.Object rsp) = fromJson =<< H.lookup idKey rsp
rspToIntId _ = Nothing

rspToCharResult :: A.Value -> Maybe Char
rspToCharResult (A.Object rsp) = fromJson =<< H.lookup resultKey rsp
rspToCharResult _ = Nothing

lockRequest :: Int -> A.Value
lockRequest i = request (Just $ A.Number $ fromIntegral i) "lock" $ Just A.emptyObject

unlockRequest :: Char -> A.Value
unlockRequest ch = request Nothing "unlock" $ Just $ A.object ["value" .= ch]

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

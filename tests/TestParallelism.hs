{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TestParallelism (testParallelizingTasks) where

import qualified Network.JsonRpc.Server as S
import Network.JsonRpc.Server ((:+:) (..))
import Internal
import Data.List (sortBy, permutations)
import Data.Function (on)
import qualified Data.Aeson as A
import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A
import Data.Maybe (fromJust)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Test.HUnit (Assertion, assert)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- | Tests parallelizing a batch request.  Each request either
--   locks or unlocks an MVar.  The MVar is initially locked,
--   so the first lock request cannot succeed if the requests
--   are serialized.
testParallelizingTasks :: Assertion
testParallelizingTasks = do
  methods <- createMethods <$> newEmptyMVar
  output <- S.callWithBatchStrategy parallelize methods input
  let rsp = fromJust $ A.decode =<< output
  assert $ elem (sortBy (compare `on` rspToIdString) rsp) possibleResponses
    where input = A.encode [ lockRequest 1
                           , lockRequest 3
                           , unlockRequest 'C'
                           , unlockRequest 'B'
                           , lockRequest 2
                           , unlockRequest 'A']
          createMethods lock = [lockMethod lock, unlockMethod lock]

possibleResponses :: [[A.Value]]
possibleResponses = (rsp <$>) <$> perms
    where perms = map (zip [1, 2, 3]) $ permutations ["A", "B", "C"]
          rsp (i, r) = defaultRsp `result` A.String r `id'` Just (A.Number i)

lockRequest :: Int -> A.Value
lockRequest i = request (Just $ A.Number $ fromIntegral i) "lock" $ Just A.emptyObject

unlockRequest :: Char -> A.Value
unlockRequest ch = request Nothing "unlock" $ Just $ A.object ["value" .= ch]

lockMethod :: MVar Char -> S.Method IO
lockMethod lock = S.toMethod "lock" f ()
    where f :: S.RpcResult IO Char
          f = liftIO $ takeMVar lock

unlockMethod :: MVar Char -> S.Method IO
unlockMethod lock = S.toMethod "unlock" f (S.Required "value" :+: ())
    where f :: Char -> S.RpcResult IO ()
          f val = liftIO $ putMVar lock val

parallelize :: [IO a] -> IO [a]
parallelize tasks = mapM takeMVar =<< mapM fork tasks
      where fork t = do
                      mvar <- newEmptyMVar
                      _ <- forkIO $ putMVar mvar =<< t
                      return mvar

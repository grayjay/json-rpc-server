{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JsonRpc.Server
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

main :: IO ()
main = do
  contents <- B.getContents
  count <- newMVar 0
  forM_ (B.lines contents) $ \request -> do
         response <- runReaderT (call methods request) count
         B.putStrLn $ fromMaybe "" response

type Server = ReaderT (MVar Integer) IO

methods :: Methods Server
methods = toMethods [add, printSequence, increment]

add, printSequence, increment :: Method Server

add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = liftIO $ return (x + y)

printSequence = toMethod "print_sequence" f params
    where params = Required "string" :+:
                   Optional "count" 1 :+:
                   Optional "separator" ',' :+: ()
          f :: String -> Int -> Char -> RpcResult Server ()
          f str count sep = do
              when (count < 0) $ throwError negativeCount
              liftIO $ print $ intercalate [sep] $ replicate count str
          negativeCount = rpcError (-32000) "negative count"

increment = toMethod "increment_and_get_count" f ()
    where f :: RpcResult Server Integer
          f = ask >>= \count -> liftIO $ modifyMVar count inc
              where inc x = return (x + 1, x + 1)

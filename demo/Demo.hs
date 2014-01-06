{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.JsonRpc.Server
import Happstack.Server.SimpleHTTP( ServerPartT, simpleHTTP, nullConf
                                  , askRq, rqBody, unBody, toResponse)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Error (throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar)

main :: IO ()
main = newMVar 0 >>= \count ->
       simpleHTTP nullConf $ do
         request <- askRq
         body <- liftIO $ getBody request
         result <- runReaderT (call methods body) count
         let resultStr = fromMaybe "" result
         return $ toResponse resultStr
    where getBody r = unBody `fmap` readMVar (rqBody r)

type Server = ReaderT (MVar Integer) (ServerPartT IO)

methods :: Methods Server
methods = toMethods [printSequence, getCount, add]

printSequence, getCount, add :: Method Server

printSequence = toMethod "print" f params
    where params = Required "string" :+:
                   Optional "count" 1 :+:
                   Optional "separator" ',' :+: ()
          f :: String -> Int -> Char -> RpcResult Server ()
          f str count sep = do
              when (count < 0) $ throwError negativeCount
              liftIO $ print $ intercalate [sep] $ replicate count str
          negativeCount = rpcError (-32000) "negative count"

getCount = toMethod "get_count" f ()
    where f :: RpcResult Server Integer
          f = ask >>= \count -> liftIO $ modifyMVar count inc
              where inc x = return (x + 1, x + 1)

add = toMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Double -> Double -> RpcResult Server Double
          f x y = liftIO $ return (x + y)

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.JsonRpc.Server
import Happstack.Server.SimpleHTTP hiding (body, result)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Concurrent.MVar

main :: IO ()
main = do
  count <- newMVar 0
  simpleHTTP nullConf $ do
         request <- askRq
         body <- liftIO $ getBody request
         result <- runReaderT (call methods body) count
         let resultStr = fromMaybe "" result
         return $ toResponse resultStr
      where getBody r = unBody `fmap` readMVar (rqBody r)

type Server = ReaderT (MVar Int) (ServerPartT IO)

methods :: JsonMethods Server
methods = toJsonMethods [printSequence, getCount, add]

printSequence, getCount, add :: JsonMethod Server

printSequence = toJsonMethod "print" f params
    where params = Required "string" :+:
                   Optional "count" 1 :+:
                   Optional "separator" ',' :+: ()
          f :: String -> Int -> Char -> RpcResult Server ()
          f str c s = liftIO $ print $ intercalate [s] $ replicate c str

getCount = toJsonMethod "get_count" f ()
    where f :: RpcResult Server Int
          f = ask >>= \c -> liftIO $ modifyMVar c inc
              where inc x = return (x + 1, x + 1)

add = toJsonMethod "add" f (Required "x" :+: Required "y" :+: ())
    where f :: Int -> Int -> RpcResult Server Int
          f x y = liftIO $ return (x + y)

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.JsonRpc.Server
import Happstack.Server.SimpleHTTP hiding (body, result)
import Data.List (intercalate)
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
         let resultStr = maybe "" id result
         return $ toResponse resultStr
      where getBody r = unBody `fmap` (readMVar $ rqBody r)

type Server = ReaderT (MVar Int) (ServerPartT IO)

methods :: JsonMethods Server
methods = toJsonMethods [printMethod, getCountMethod, addMethod]

printMethod, getCountMethod, addMethod :: JsonMethod Server

printMethod = toJsonMethod "print" f params
    where params = Required "string" :+:
                   Optional "count" 1 :+:
                   Optional "separator" ',' :+: ()
          f :: String -> Int -> Char -> RpcResult Server ()
          f str c s = liftIO $ print $ intercalate [s] $ replicate c str

getCountMethod = toJsonMethod "get_count" getCount ()
    where getCount :: RpcResult Server Int
          getCount = do
            counter <- ask
            count <- liftIO $ modifyMVar counter $ \old ->
                     let new = old + 1
                     in return (new, new)
            liftIO $ print count
            return count

addMethod = toJsonMethod "add" add $ (Required "x" :+: Required "y" :+: ())
    where add :: Int -> Int -> RpcResult Server Int
          add x y = liftIO $ return (x + y)

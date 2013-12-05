{-# LANGUAGE OverloadedStrings #-}

import Data.JsonRpc.Server
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [testCase "invalid JSON" testInvalidJson]

emptyJsonFunctions :: Monad m => JsonFunctions m
emptyJsonFunctions = toJsonFunctions []

testInvalidJson :: Assertion
testInvalidJson = checkCode `liftM` result @? "Failed to detect invalid JSON"
                  where result :: IO (Maybe B.ByteString)
                        result = call emptyJsonFunctions "5"
                        checkCode r = case r of
                                Nothing -> False
                                Just json -> "-32700" `isInfixOf` (B.unpack json)

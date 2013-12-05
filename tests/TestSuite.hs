{-# LANGUAGE OverloadedStrings #-}

import Data.JsonRpc.Common
import Data.JsonRpc.Server
import Data.Aeson
import Data.List
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [testCase "invalid JSON" testInvalidJson]

emptyJsonFunctions :: Monad m => JsonFunctions m
emptyJsonFunctions = toJsonFunctions []

testInvalidJson :: Assertion
testInvalidJson = (runIdentity $ getCode `liftM` result) @?= Just (-32700)
                  where result :: Identity (Maybe Response)
                        result = (fromByteString =<<) <$> call emptyJsonFunctions "5"
                        getCode r = case r of
                                      Just (Response _ (Left (RpcError code _ _))) -> Just code
                                      _ -> Nothing

fromByteString x = case fromJSON <$> decode x of
                     Just (Success x') -> Just x'
                     _ -> Nothing

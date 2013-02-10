module Main where

import qualified Tests.RestApi as Rest
import Test.Framework (defaultMain, testGroup)

main :: IO ()
main = defaultMain tests
    where tests = [testGroup "encoding-mashup-server" Rest.tests]

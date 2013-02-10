module Tests.RestApi (tests) where

import RestApi
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


tests :: [Test]
tests = [testGetAllChars]


testGetAllChars :: Test
testGetAllChars = testCase "RestApi/testGetAllChars" $ do
    assertEqual "test" 1 1

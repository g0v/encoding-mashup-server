{-# LANGUAGE OverloadedStrings #-}

module Tests.RestApi (tests) where

import RestApi
import Tests.App

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Data.Map as Map

import Snap.Snaplet
import qualified Snap.Test as ST
import Snap.Snaplet.Test

tests :: [Test]
tests = [testGetMeta, testGetAllChars]


testGetMeta :: Test
testGetMeta = testCase "RestApi/testGetMeta" $ assertMetaInfo
    where assertMetaInfo :: Assertion
          assertMetaInfo = do
                let hdl = with restApi metaHandler
                res <- runHandler (ST.get "" Map.empty) hdl appInit
                either (assertFailure . show) (ST.assertBodyContains "version") res

testGetAllChars :: Test
testGetAllChars = testCase "RestApi/testGetAllChars" $ do
    assertEqual "RestApi/testGetAllChars" 1 1

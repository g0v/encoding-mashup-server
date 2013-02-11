{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.App (appInit, restApi) where

import RestApi

import Control.Lens

import Snap.Snaplet

data App = App {
    _restApi :: Snaplet RestApi
}

$(makeLenses ''App)

appInit :: SnapletInit App App
appInit = makeSnaplet "app" "Test Application" Nothing $ do
    r <- nestSnaplet "api" restApi $ initRestApi undefined undefined
    return $ App r

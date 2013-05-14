{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Site where

import Control.Lens

import Snap

import CharDatabase
import EncodingTable
import WebUi
import RestApi

data App = App
  { _charDatabase  :: Snaplet CharDatabase
  , _encodingTable :: Snaplet EncodingTable
  , _webUi         :: Snaplet WebUi
  , _restApi       :: Snaplet RestApi
  }

$(makeLenses ''App)

initApp :: SnapletInit App App
initApp = makeSnaplet "app" "萌典校正系統" Nothing $ do
  cs <- nestSnaplet "db"  charDatabase    initCharDatabase
  es <- nestSnaplet "tbl" encodingTable   initEncodingTable
  ws <- nestSnaplet "web" webUi           initWebUi
  as <- nestSnaplet "api" restApi       $ initRestApi cs es
  return $ App cs es ws as


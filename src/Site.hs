{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Site where

import Control.Lens
import System.FilePath ( (</>) )

import Snap
import Snap.Util.FileServe

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
  addRoutes [("", indexHandler)]
  cs <- nestSnaplet "db"  charDatabase    initCharDatabase
  es <- nestSnaplet "tbl" encodingTable   initEncodingTable
  ws <- nestSnaplet "web" webUi           initWebUi
  as <- nestSnaplet "api" restApi       $ initRestApi cs es
  return $ App cs es ws as

indexHandler :: Handler b App ()
indexHandler = do
  snapletDir <- getSnapletFilePath
  serveFile $ snapletDir </> "snaplets" </> "web-ui" </> "static" </> "index.html"

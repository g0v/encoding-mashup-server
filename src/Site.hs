{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Site where

import Control.Lens

import Snap
import Snap.Util.FileServe

import CharDatabase
import EncodingTable
import RestApi

data App = App
  { _charDatabase  :: Snaplet CharDatabase
  , _encodingTable :: Snaplet EncodingTable
  , _restApi       :: Snaplet RestApi
  }

$(makeLenses ''App)

initApp :: SnapletInit App App
initApp = makeSnaplet "app" "萌典校正系統" Nothing $ do
  cs <- nestSnaplet "db"     charDatabase   $ initCharDatabase
  es <- nestSnaplet "encode" encodingTable  $ initEncodingTable
  as <- nestSnaplet "api"    restApi        $ initRestApi cs es
  addRoutes [("static", serveDirectory "static")]
  return $ App cs es as


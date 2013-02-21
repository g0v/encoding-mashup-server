{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebUi
  (
    WebUi
  , initWebUi
  ) where

import           Data.String
import           System.FilePath
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Util

data WebUi = WebUi

initWebUi :: SnapletInit b WebUi
initWebUi = makeSnaplet name description getData $ do
  snapletDir <- getSnapletFilePath
  addRoutes [("", serveDirectory $ snapletDir </> "static")]
  return WebUi
  where
    name :: IsString a => a
    name = "web-ui"
    description = "Web-based User Interface"
    getData = Just $ getResourceDir name

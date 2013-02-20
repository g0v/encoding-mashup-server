{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module WebUi
  (
    WebUi
  , initWebUi
  ) where

import           System.FilePath
------------------------------------------------------------------------------
import           Snap.Snaplet
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Utils

data WebUi = WebUi

initWebUi :: SnapletInit b WebUi
initWebUi = makeSnaplet name description getData $ do
  snapletDir <- getSnapletFilePath
  addRoutes [("", serveDirectory $ snapletDir </> "static")]
  return WebUi
  where
    name = "wui"
    description = "Web-based User Interface"
    getData = Just $ getResourceDir "wui"

{-# LANGUAGE OverloadedStrings #-}
module WebUi
  (
    WebUi
  , initWebUi
  ) where

import           Data.String
import           Data.ByteString
import           System.FilePath
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Clay
------------------------------------------------------------------------------
import           Snap
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Util
import           HttpUtil

data WebUi = WebUi

initWebUi :: SnapletInit b WebUi
initWebUi = makeSnaplet name description getData $ do
  addRoutes [ ("css", pathArg cssHandler)
            , ("",            staticHandler)
            ]
  return WebUi
  where
    name :: IsString a => a
    name = "web-ui"
    description = "Web-based User Interface"
    getData = Just $ getResourceDir name

staticHandler :: Handler b WebUi ()
staticHandler = do
  snapletDir <- getSnapletFilePath
  serveDirectory $ snapletDir </> "static"

cssHandler :: ByteString -> Handler b WebUi ()
cssHandler fp = case fp of
  "encoding-mashup.css" -> finishWithCss encodingMashupCss
  _                     -> pass
  where
    cssMime = "text/css"

    finishWithCss css = do
      let out = encodeUtf8 $ renderWith compact [] css
      let tag = Just $ etag out
      checkMatch tag
      finishWithLBS cssMime out tag

encodingMashupCss :: Css
encodingMashupCss = do 
  star # byClass "char-info" ? do
    margin 2 2 2 2
    height $ px 40
    float sideLeft

  star # byClass "char-info-display" ? do
    float sideLeft 
    fontSize $ px 24
    lineHeight $ px 24
    height $ px 30
    width $ px 100
    border solid (px 2) black
    borderRadius $ px 8

  input ? do
    float sideLeft

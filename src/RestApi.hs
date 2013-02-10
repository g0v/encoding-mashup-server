{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import Prelude hiding (getChar)

import Data.Maybe
import Data.Text.Encoding
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Aeson.TH
import Control.Lens hiding ((.=))

import Snap

import Type hiding (_timestamp, timestamp)
import CharDatabase
import EncodingTable

data CharFilter = CharFilter
  { _timestamp :: Integer
  , _charNames :: [CharName]
  }
$(makeLenses ''CharFilter)
$(deriveJSON (drop 1) ''CharFilter)

data RestApi = RestApi
  { _charDatabase         :: Snaplet CharDatabase
  , _encodingTable        :: Snaplet EncodingTable
  }
$(makeLenses ''RestApi)

version :: ByteString
version = "0.0"

withUniqueCapture :: ByteString -> (ByteString -> Handler b v a) -> Handler b v a
withUniqueCapture name handler = do
  names <- join <$> maybeToList <$> rqParam name <$> getRequest
  case names of
    [val] -> handler val
    _     -> logError "Capturing failed." >> pass

initRestApi :: Snaplet CharDatabase
            -> Snaplet EncodingTable
            -> SnapletInit b RestApi
initRestApi cs es = makeSnaplet "rest-api" "JSON 介面" Nothing $ do
  addRoutes [ ("meta",                 metaHandler)
            , ("char/:uri",            withUniqueCapture "uri" charHandler)
            , ("chars/all",            allCharsHandler)
            , ("chars/updated",        updatedCharsHandler)
            , ("lookup/cns/:char",     withUniqueCapture "char" lookupCnsHandler)
            , ("lookup/unicode/:code", withUniqueCapture "code" lookupUnicodeHandler)
            ]
  return $ RestApi cs es

writeJson :: ToJSON a => a -> Handler b v ()
writeJson = writeLBS . encode . toJSON

readJson :: FromJSON a => Handler b v a
readJson = fromMaybe error' <$> decode <$> readRequestBody 2048
  where error' = error "JSON decoding failed."

metaHandler :: Handler b v ()
metaHandler = methods [GET, HEAD] $ writeJson $ object ["version" .= version]

charHandler :: ByteString -> Handler b RestApi ()
charHandler charName' = with charDatabase $
  methods [GET, HEAD] getter  <|>
  method  PUT         setter  <|>
  method  DELETE      deleter
  where
    charName = decodeUtf8 charName'
    getter   = writeJson =<< getChar charName
    setter   = updateChar charName =<< readJson
    deleter  = deleteChar charName

allCharsHandler :: Handler b RestApi ()
allCharsHandler = methods [GET, HEAD] $ writeJson =<< with charDatabase getChars

updatedCharsHandler :: Handler b RestApi ()
updatedCharsHandler = methods [GET, HEAD] . with charDatabase $ do
  cond <- readJson
  writeJson =<< getUpdatedChars (cond^.timestamp) (cond^.charNames)

lookupCnsHandler :: ByteString -> Handler b RestApi ()
lookupCnsHandler unicode' = methods [GET, HEAD] . with encodingTable $
  writeJson =<< makeJson <$> lookupCns (decodeUtf8 unicode')
  where makeJson x = object ["unicode" .= x]

lookupUnicodeHandler :: ByteString -> Handler b RestApi ()
lookupUnicodeHandler cns' = methods [GET, HEAD] . with encodingTable $
  writeJson =<< makeJson <$> lookupUnicode (decodeUtf8 cns')
  where makeJson x = object ["cns" .= x]

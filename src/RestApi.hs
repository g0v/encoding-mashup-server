{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import Prelude hiding (getChar)

import Data.Text.Encoding as E
import Data.ByteString (ByteString)
import Data.Aeson
import Control.Lens hiding ((.=))

import Snap

import Type
import Config
import CharDatabase
import EncodingTable

data RestApi b = RestApi
  { _charDatabase   :: SnapletLens b CharDatabase
  , _encodingTable  :: SnapletLens b EncodingTable
  }

$(makeLenses ''RestApi)

initRestApi :: SnapletLens b CharDatabase
            -> SnapletLens b EncodingTable
            -> SnapletInit b (RestApi b)
initRestApi cs es = makeSnaplet "api" "JSON 介面" Nothing $ do
  addRoutes [ ("meta", metaHandler)
            , ("char/:source", charHandler)
            ]
  return $ RestApi cs es

version :: ByteString
version = "0.0"

writeJson :: Value -> Handler b v ()
writeJson = writeLBS . encode

metaHandler :: Handler b (RestApi b) ()
metaHandler = methods [GET, HEAD] $ writeJson $ object ["version" .= version]

getCharNameFromParam :: ByteString -> Handler b (RestApi b) CharName
getCharNameFromParam capture = do
  names <- rqParam capture <$> getRequest
  case names of
    Just [n] -> return $ decodeUtf8 n
    _        -> pass

withCharDatabase :: Handler b CharDatabase a -> Handler b (RestApi b) a
withCharDatabase f = use charDatabase >>= (`withTop` f)

charHandler :: Handler b (RestApi b) ()
charHandler = charHandler' =<< getCharNameFromParam "source"

charHandler' :: CharName -> Handler b (RestApi b) ()
charHandler' charName = withCharDatabase $
  methods [GET, HEAD] getter <|> method PUT setter <|> method DELETE deleter
  where
    getter = writeJson =<< toJSON <$> getChar charName
    setter = maybe pass (updateChar charName) =<< decode <$> readRequestBody maxRequestBodyLength
    deleter = deleteChar charName 


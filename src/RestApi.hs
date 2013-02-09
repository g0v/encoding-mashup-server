{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import Prelude hiding (getChar)

import Data.ByteString (ByteString)
import Data.Aeson
import Control.Lens.TH

import Snap

import Type
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
            ]
  return $ RestApi cs es

version :: ByteString
version = "0.0"

metaHandler :: Handler b (RestApi b) ()
metaHandler = method GET $ writeLBS $ encode $ object ["version" .= version]


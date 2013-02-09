{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens

import Snap

import CharDatabase
import EncodingTable
import RestApi

data Server = Server
  { _charDatabase  :: Snaplet CharDatabase
  , _encodingTable :: Snaplet EncodingTable
  , _restApi       :: Snaplet RestApi
  }

$(makeLenses ''Server)

initServer :: SnapletInit Server Server
initServer = makeSnaplet "server" "萌典校正系統" Nothing $ do
  cs <- nestSnaplet "db"     charDatabase   $ initCharDatabase
  es <- nestSnaplet "encode" encodingTable  $ initEncodingTable
  as <- nestSnaplet "api"    restApi        $ initRestApi cs es
  return $ Server cs es as

main :: IO ()
main = serveSnaplet defaultConfig initServer

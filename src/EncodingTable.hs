{-# LANGUAGE OverloadedStrings #-}
module EncodingTable where

import Data.Text (Text)
import Snap

import Type

data EncodingTable = EncodingTable

initEncodingTable :: SnapletInit b EncodingTable
initEncodingTable = makeSnaplet "encodingTable" "Unicode 和 CNS 查詢介面" Nothing $ do
  return $ EncodingTable

lookupUnicode :: CNSCode -> Handler b EncodingTable Text
lookupUnicode = undefined

lookupCNS :: Text -> Handler b EncodingTable CNSCode
lookupCNS = undefined

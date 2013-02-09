{-# LANGUAGE OverloadedStrings #-}
module EncodingTable where

import Data.Text (Text)
import Snap

import Type

data EncodingTable = EncodingTable

initEncodingTable :: SnapletInit b EncodingTable
initEncodingTable = makeSnaplet "encodingTable" "Unicode 和 Cns 表格" Nothing $ do
  return $ EncodingTable

lookupUnicode :: CnsCode -> Handler b EncodingTable Text
lookupUnicode = undefined

lookupCns :: Text -> Handler b EncodingTable CnsCode
lookupCns = undefined

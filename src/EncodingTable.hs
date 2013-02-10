{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module EncodingTable where

import Data.HashMap.Strict (HashMap, empty, fromList)
import Data.Text (Text)
import Control.Lens
import Snap.Core
import Snap.Snaplet

import Type

data EncodingTable = EncodingTable
  { _cnsTable :: HashMap CnsCode Text
  , _uniTable :: HashMap Text CnsCode
  }
$(makeLenses ''EncodingTable)

initEncodingTable :: SnapletInit b EncodingTable
initEncodingTable = makeSnaplet "encoding-table" "Unicode 和 Cns 表格" Nothing $ do
  return $ EncodingTable empty empty

lookupUnicode :: CnsCode -> Handler b EncodingTable (Maybe Text)
lookupUnicode uni = use $ uniTable.at uni

lookupCns :: Text -> Handler b EncodingTable (Maybe CnsCode)
lookupCns cns = use $ cnsTable.at cns

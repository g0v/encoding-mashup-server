module EncodingTable where

import Data.Text (Text)
import Snap

import Type

type EncodingTable = ()

initEncodingTable :: SnapletInit b EncodingTable
initEncodingTable = undefined

lookupUnicode :: CNSCode -> Handler b EncodingTable Text
lookupUnicode = undefined

lookupCNS :: Text -> Handler b EncodingTable CNSCode
lookupCNS = undefined

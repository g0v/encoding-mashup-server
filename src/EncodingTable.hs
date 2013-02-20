{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module EncodingTable
  (
    EncodingTable
  , initEncodingTable
  , cnsCodeToUniChar
  , uniCharToCnsCode
  , isBlackListed
  ) where

import           System.FilePath
import           Data.String
import           Data.Maybe
import           Control.Applicative
import qualified Data.ByteString.Lazy as LB
import           Data.HashMap.Strict (HashMap)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Control.Lens
------------------------------------------------------------------------------
import           Snap.Snaplet
------------------------------------------------------------------------------
import           Type
import           Utils

data EncodingTable = EncodingTable
  { _cns2uniTable :: HashMap CnsCode UniChar
  , _uni2cnsTable :: HashMap UniChar CnsCode
  }
$(makeLenses ''EncodingTable)

initEncodingTable :: SnapletInit b EncodingTable
initEncodingTable = makeSnaplet name description getData $ do
  snapletDir <- getSnapletFilePath
  cns2uni <- liftIO $ fromJust . decode' <$> LB.readFile (snapletDir </> "cns2uni.json")
  uni2cns <- liftIO $ fromJust . decode' <$> LB.readFile (snapletDir </> "uni2cns.json")
  return $ EncodingTable cns2uni uni2cns
  where
    name :: IsString a => a
    name = "encoding-table"
    description = "Unicode 和 Cns 表格"
    getData = Just $ getResourceDir name

-- | CNS-11643 code to Unicode code point(s).
cnsCodeToUniChar :: CnsCode -> Handler b EncodingTable (Maybe UniChar)
cnsCodeToUniChar code = use $ uni2cnsTable.at code

-- | Unicode code point(s) to CNS-11643 code.
uniCharToCnsCode :: UniChar -> Handler b EncodingTable (Maybe CnsCode)
uniCharToCnsCode char = use $ cns2uniTable.at char

-- | (NOT IMPLEMENTED YET!)
--   A function to test whether the leading code point is a compatible ideograph.
isBlackListed :: UniChar -> Bool
isBlackListed _ = False

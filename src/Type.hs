{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Type where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.HashMap.Strict (HashMap)
import           Data.Aeson
import           Data.Aeson.TH
import           Control.Applicative
import           Control.Monad
import           Control.Lens (view)
import           Control.Lens.TH

type CharName = ByteString
type CnsCode = Text

-- | 可能有 IVD, 所以不用 Char
type UniChar = Text

-- | The type of a strong Etag.
type Etag = ByteString

-- | The type of a base MIME. Subjects to changes.
type Mime = ByteString


-- | 字元顯示資訊
data CharDisplay = CharDisplay
  { _uni :: Maybe UniChar -- ^ Unicode 認同碼
  , _ids :: Maybe UniChar -- ^ Unicode 描述字串
  , _pua :: Maybe UniChar -- ^ 使用者造字
  } deriving Show
$(makeLenses ''CharDisplay)
$(deriveJSON (drop 1) ''CharDisplay)

-- | 字元顯示資訊
data CharExact = CharExact
  { _cns :: Maybe CnsCode
  , _forcedUni :: Maybe Text
  } deriving Show
$(makeLenses ''CharExact)

instance FromJSON CharExact where
   parseJSON (Object v) = CharExact         <$>
                          v .: "cns"        <*>
                          v .: "forced_uni"
   parseJSON _          = mzero

instance ToJSON CharExact where
   toJSON charexact = object
                        [ "cns"        .= view cns       charexact
                        , "forced_uni" .= view forcedUni charexact
                        ]

-- | 字元資訊
data CharInfo = CharInfo
  { _hidden :: !Bool         -- ^ 從前端介面中隱藏
  , _tabled :: !Bool         -- ^ 有爭議、沒有好答案、沒有好編碼的字
  , _display :: !CharDisplay -- ^ 顯示用資訊
  , _exact :: !CharExact     -- ^ 精確資訊
  , _comment :: !Text        -- ^ 註解
  } deriving Show
$(makeLenses ''CharInfo)
$(deriveJSON (drop 1) ''CharInfo)

-- | CharInfo Map
type CharMap = HashMap CharName CharInfo

-- | Etag Map
type EtagMap = HashMap CharName Etag

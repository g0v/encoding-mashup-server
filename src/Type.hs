{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Type where

import Data.Text (Text)
import Data.Int (Int64)
import Data.Map (Map)
import Control.Lens.TH
import Data.Aeson.TH

type CNSCode = Text

-- | 字元顯示資訊
data CharDisplay = CharDisplay
  { _uni :: String -- ^ Unicode 認同碼
  , _ids :: String -- ^ Unicode 描述字串
  , _pua :: String -- ^ 使用者造字
  }

$(makeLenses ''CharDisplay)
$(deriveJSON (drop 1) ''CharDisplay)

-- | 字元顯示資訊
data CharExact = CharExact
  { _cns :: Maybe CNSCode
  , _manualUni :: Maybe Text
  }

$(makeLenses ''CharExact)
$(deriveJSON (drop 1) ''CharExact)

-- | 字元資訊
data CharInfo = CharInfo
  { _hidden :: Bool         -- ^ 從前端介面中隱藏
  , _tabled :: Bool         -- ^ 有爭議、沒有好答案、沒有好編碼的字
  , _display :: CharDisplay -- ^ 顯示用資訊
  , _exact :: CharExact     -- ^ 精確資訊
  , _comments :: Text       -- ^ 註解
  , _checked :: Int64       -- ^ 資料存取過的次數
  , _timestamp :: Int64     -- ^ 時間。 JSON 的時間很亂，所以採用 Unix 時間
  }

$(makeLenses ''CharInfo)
$(deriveJSON (drop 1) ''CharInfo)

type CharName = Text
type CharMap = Map CharName CharInfo
type NullableCharMap = Map CharName (Maybe CharInfo)


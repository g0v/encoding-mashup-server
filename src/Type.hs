{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Type where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Control.Lens.TH

type CharName = ByteString
type CnsCode = Text
type UniChar = Text -- 可能有 IVD, 所以不用 Char
type Etag = ByteString

-- | 字元顯示資訊
data CharDisplay = CharDisplay
  { _uni :: Maybe UniChar -- ^ Unicode 認同碼
  , _ids :: Maybe UniChar -- ^ Unicode 描述字串
  , _pua :: Maybe UniChar -- ^ 使用者造字
  }

$(makeLenses ''CharDisplay)

-- | 字元顯示資訊
data CharExact = CharExact
  { _cns :: Maybe CnsCode
  , _forcedUni :: Maybe Text
  }

$(makeLenses ''CharExact)

-- | 字元資訊
data CharInfo = CharInfo
  { _hidden :: !Bool         -- ^ 從前端介面中隱藏
  , _tabled :: !Bool         -- ^ 有爭議、沒有好答案、沒有好編碼的字
  , _display :: !CharDisplay -- ^ 顯示用資訊
  , _exact :: !CharExact     -- ^ 精確資訊
  , _comment :: !Text        -- ^ 註解
  }

$(makeLenses ''CharInfo)

type CharMap = HashMap CharName CharInfo
type EtagMap = HashMap CharName Etag

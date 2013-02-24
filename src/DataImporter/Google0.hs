{-# LANGUAGE OverloadedStrings #-}

module DataImporter.Google0
  ( parseGoogle0
  ) where

import           Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import           Data.Csv
import           Control.Monad

import           Type

data Google0Record = Google0Record { unwrap :: (CharName, CharInfo) }

instance FromRecord Google0Record where
  parseRecord v
    | V.length v == 19 = do
        charname'         <- liftM ("moe/revised/" `B.append`) $ v .! 0
        tabled'           <- liftM (not . B.null) $ v .! 2
        display_uni'      <- v .! 4
        display_ids'      <- v .! 5
        exact_cns'        <- v .! 11
        exact_forcedUni'  <- v .! 13
        comment'          <- v .! 18
        return . Google0Record . (,) charname' $ CharInfo
          { _hidden = False
          , _tabled = tabled'
          , _display = CharDisplay
            { _uni = display_uni'
            , _ids = display_ids'
            , _pua = Nothing
            }
          , _exact = CharExact
            { _cns       = exact_cns'
            , _forcedUni = exact_forcedUni'
            }
          , _comment = comment'
          }
    | otherwise     = mzero

parseGoogle0 :: LB.ByteString -> Maybe [(CharName, CharInfo)]
parseGoogle0 str =
  case decode False str of
    Left  _    -> Nothing
    Right list -> Just . toL . V.drop 3 $ list
  where
    toL = map unwrap . V.toList

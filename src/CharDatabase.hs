{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module CharDatabase where

import Control.Applicative
import Control.Monad (forM)
import Control.Lens

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as H

import Database.SQLite.Simple.FromRow

import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Type

data CharPair = CharPair CharName CharInfo

unwrapCharPair :: CharPair -> (CharName, CharInfo)
unwrapCharPair (CharPair a b) = (a, b)

instance FromRow CharPair where
    fromRow = CharPair <$> field
                       <*> (CharInfo <$> field
                                     <*> field
                                     <*> (CharDisplay <$> field
                                                      <*> field
                                                      <*> field)
                                     <*> (CharExact <$> field
                                                    <*> field)
                                     <*> field
                                     <*> field
                                     <*> field)

data CharDatabase = CharDatabase
  { _db :: Snaplet Sqlite
  }
$(makeLenses ''CharDatabase)

initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "char-database" "字元資料庫" Nothing $ do
    d <- nestSnaplet "db" db sqliteInit
    return $ CharDatabase d

getChars :: Handler b CharDatabase CharMap
getChars = H.fromList . map unwrapCharPair <$> access
  where access = with db $ query_ "select * from char_info"

getChar :: CharName -> Handler b CharDatabase (Maybe CharInfo)
getChar c = listToMaybe . map (snd . unwrapCharPair) <$> access
  where access = with db $ query "select * from char_info where charname = ?" $ Only c

updateChar :: CharName -> CharInfo -> Handler b CharDatabase ()
updateChar cn ci = undefined undefined

deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar = undefined

getUpdatedChars :: Integer -> [CharName] -> Handler b CharDatabase NullableCharMap
getUpdatedChars = undefined

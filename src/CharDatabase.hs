{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module CharDatabase where

import Control.Applicative
import Control.Monad (forM)
import Control.Lens

import Data.Maybe (listToMaybe)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as H

import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField

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

instance ToRow CharPair where
    toRow cp = let cn = fst . unwrapCharPair $ cp
                   ci = snd . unwrapCharPair $ cp
               in [
                 toField cn,
                 ci ^. hidden . to toField,
                 ci ^. tabled . to toField,
                 ci ^. display . uni . to toField,
                 ci ^. display . ids . to toField,
                 ci ^. display . pua . to toField,
                 ci ^. exact . cns . to toField,
                 ci ^. exact . manualUni . to toField,
                 ci ^. comments . to toField,
                 ci ^. checked . to toField,
                 ci ^. timestamp . to toField
               ]

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
updateChar cn ci = do
    let cp = CharPair cn ci
    {- check out http://stackoverflow.com/questions/2717590/sqlite-upsert-on-duplicate-key-update -} 
    with db $ execute "INSERT OR IGNORE INTO char_info (charname, hidden, tabled, display_uni, display_ids, display_pua, exact_cns, exact_manualuni, comments, checked, timestamp) VALUES (?,?,?,?,?,?,?,?,?,?,?)" cp
    with db $ execute "UPDATE char_info SET hidden=?, tabled=?, display_uni=?, display_ids=?, display_pua=?, exact_cns=?, exact_manualuni=?, comments=?, checked=? where charname LIKE ?" ((ci ^. hidden), (ci ^. tabled), (ci ^. display . uni), (ci ^. display . ids), (ci ^. display. pua), (ci ^. exact . cns), (ci ^. exact . manualUni), (ci ^. comments), (ci ^. checked), cn)
    with db $ execute "UPDATE char_info SET timestamp=? where charname LIKE ?" ((ci ^. timestamp), cn)

deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar cn = do
    with db $ execute "DELETE FROM char_info WHERE charname=?" $ Only cn


getUpdatedChars :: Integer -> [CharName] -> Handler b CharDatabase CharMap
getUpdatedChars timestamp limit = do
    {- FIXME: quick & arbitrary implementation -}
    H.fromList . map unwrapCharPair <$> access
    where access = with db $ query  "select * from char_info where timestamp > ? ORDER BY timestamp DSC LIMIT 10" $ Only timestamp

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module CharDatabase where

import Control.Applicative
import Control.Lens

import Data.Maybe (listToMaybe)
import qualified Data.HashMap.Strict as H

import Database.PostgreSQL.Simple.FromRow

import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple

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
                                     <*> field)


data CharDatabase = CharDatabase { 
                        _db :: Snaplet Postgres
                    }

$(makeLenses ''CharDatabase)

initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "char-database" "字元資料庫" Nothing $ do
    d <- nestSnaplet "db" db pgsInit
    return $ CharDatabase d

getChars :: Handler b CharDatabase CharMap
getChars = H.fromList . map unwrapCharPair <$> access
    where access = with db $ query_ "select * from char_info where status = 'exist'"

getChar :: CharName -> Handler b CharDatabase (Maybe CharInfo)
getChar c = listToMaybe . map (snd . unwrapCharPair) <$> access
    where access = with db $ query "select * from char_info where charname = ? AND status = 'exist'" $ Only c

updateChar :: CharName -> CharInfo -> Handler b CharDatabase ()
updateChar cn ci = do 
    let cp = CharPair cn ci
    with db $ execute_ "begin;"
    with db $ execute "update char_info set hidden=?, tabled=?, display_uni=?, display_ids=?, display_pua=?, exact_cns=?, exact_forceduni=?, comment=? where charname like ?" (ci ^. hidden, ci ^. tabled, ci ^. display. uni, ci ^. display . ids, ci ^. display . pua, ci ^. exact . cns, ci ^. exact . forcedUni, ci ^. comment, cn)
    with db $ execute "insert into char_info (hidden, tabled, display_uni, display_ids, display_pua, exact_cns, exact_manualuni, comment) select ?, ?, ?, ?, ?, ?, ?, ? where not exists (select 1 from char_info where charname = ?)" $ (ci ^. hidden, ci ^. tabled, ci ^. display . uni, ci ^. display . ids, ci ^. display . pua, ci ^. exact . cns, ci ^. exact . forcedUni, ci ^. comment, cn)
    with db $ execute_ "commit;"
    return ()

deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar cn = do
    with db $ execute "update char_info set status = 'deleted' where charname = ?" $ Only cn
    return ()

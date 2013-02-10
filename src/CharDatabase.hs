{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module CharDatabase where

import Control.Lens

import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Type

data CharDatabase = CharDatabase { _db :: Snaplet Sqlite
                                 }


$(makeLenses ''CharDatabase)


initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "char-database" "字元資料庫" Nothing $ do
    d <- nestSnaplet "db" db sqliteInit
    return $ CharDatabase d

getChars :: Handler b CharDatabase CharMap
getChars = undefined

getChar :: CharName -> Handler b CharDatabase CharInfo
getChar = undefined

updateChar :: CharName -> CharInfo -> Handler b CharDatabase ()
updateChar = undefined

deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar = undefined

getUpdatedChars :: Integer -> [CharName] -> Handler b CharDatabase NullableCharMap
getUpdatedChars = undefined

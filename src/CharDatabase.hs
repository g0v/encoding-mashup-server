{-# LANGUAGE OverloadedStrings #-}
module CharDatabase where

import Snap.Snaplet

import Type

data CharDatabase = CharDatabase

initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "charDatabase" "字元資料庫" Nothing $ do
  return $ CharDatabase

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

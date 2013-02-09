module CharDatabase where

import Snap.Snaplet

import Type

type CharDatabase = ()

initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = undefined

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

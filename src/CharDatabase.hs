{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module CharDatabase where

import Snap.Snaplet
import Control.Lens

import Type

data CharDatabase = CharDatabase
$(makeLenses ''CharDatabase)

initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "char-database" "字元資料庫" Nothing $ do
    return $ CharDatabase

getChars :: Handler b CharDatabase CharMap
getChars = undefined

getChar :: CharName -> Handler b CharDatabase (Maybe CharInfo)
getChar c = undefined

updateChar :: CharName -> CharInfo -> Handler b CharDatabase ()
updateChar cn ci = undefined

deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar cn = undefined

getUpdatedChars :: [CharName] -> Handler b CharDatabase CharMap
getUpdatedChars etagmap = undefined

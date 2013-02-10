{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module CharDatabase where

import Control.Applicative
import Control.Monad (forM)
import Control.Lens

import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.HashMap.Strict as H

import Database.SQLite.Simple.FromRow

import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Type


data CharInfoRow = CharInfoRow
    { _charname_field :: CharName 
    , _hidden_field :: !Bool
    , _tabled_field :: !Bool
    , _display_uni_field :: Maybe Text
    , _display_ids_field :: Maybe Text
    , _display_pua_field :: Maybe Text
    , _exact_cns_field :: Maybe CnsCode
    , _exact_manualuni_field :: Maybe Text
    , _comments_field :: !Text
    , _checked_field :: !Int64
    , _timestamp_field :: !Int64
    }

$(makeLenses ''CharInfoRow)

instance FromRow CharInfoRow where
    fromRow = CharInfoRow <$> field <*> field <*> field <*> field <*> field
                          <*> field <*> field <*> field <*> field <*> field
                          <*> field


data CharDatabase = CharDatabase { _db :: Snaplet Sqlite
                                 }


$(makeLenses ''CharDatabase)


initCharDatabase :: SnapletInit b CharDatabase
initCharDatabase = makeSnaplet "char-database" "字元資料庫" Nothing $ do
    d <- nestSnaplet "db" db sqliteInit
    return $ CharDatabase d


getChars :: Handler b CharDatabase CharMap
getChars = do 
    rows <- with db $ query_ "select * from char_info" :: Handler b CharDatabase [CharInfoRow]
    pairs <- forM rows $ \c_row ->
                 return $ ((c_row ^. charname_field), (CharInfo (c_row ^. hidden_field) (c_row ^. tabled_field) (CharDisplay (c_row ^. display_uni_field)  (c_row ^. display_ids_field) (c_row ^. display_pua_field)) (CharExact (c_row ^. exact_cns_field) (c_row ^. exact_manualuni_field)) (c_row ^. comments_field) (c_row ^. checked_field) (c_row ^. timestamp_field)))
    return $ H.fromList pairs


getChar :: CharName -> Handler b CharDatabase (Maybe CharInfo)
getChar c = do
    rows <- with db $ query "select * from char_info where charname = ?" (Only c) :: Handler b CharDatabase [CharInfoRow]
    case null rows of
        True ->  return $ Nothing
        False -> do let c_row = head rows
                    return $ Just (CharInfo (c_row ^. hidden_field) (c_row ^. tabled_field) (CharDisplay (c_row ^. display_uni_field)  (c_row ^. display_ids_field) (c_row ^. display_pua_field)) (CharExact (c_row ^. exact_cns_field) (c_row ^. exact_manualuni_field)) (c_row ^. comments_field) (c_row ^. checked_field) (c_row ^. timestamp_field))


updateChar :: CharName -> CharInfo -> Handler b CharDatabase ()
updateChar cn ci = undefined undefined


deleteChar :: CharName -> Handler b CharDatabase ()
deleteChar = undefined

getUpdatedChars :: Integer -> [CharName] -> Handler b CharDatabase NullableCharMap
getUpdatedChars = undefined

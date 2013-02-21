{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as H
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Control.Monad
import           Control.Lens hiding ((.=))
------------------------------------------------------------------------------
import           Snap
------------------------------------------------------------------------------
import           Type
import           HttpUtils
import qualified CharDatabase as C
import qualified EncodingTable as E

------------------------------------------------------------------------------
-- Meta
------------------------------------------------------------------------------

version :: Int
version = 0

--------------------------------------------------------------------------------
-- JSON tools.
--------------------------------------------------------------------------------

jsonMime :: Mime
jsonMime = "application/json"

toLBS :: ToJSON a => a -> LB.ByteString
toLBS = encode . toJSON

fromLBS :: FromJSON a => LB.ByteString -> Handler b v a
fromLBS = errIfNothing 400 . decode'

readJson :: FromJSON a => Handler b v a
readJson = fromLBS =<< readLBS

jsonLookup :: FromJSON a => Object -> Text -> Handler b v a
jsonLookup obj = errIfNothing 400 . parseMaybe (obj .:)

checkVersion :: Object -> Handler b v ()
checkVersion obj = do
  version' <- jsonLookup obj "version"
  when (version' /= version) $ err 400

--------------------------------------------------------------------------------
-- Snaplet state and initializer.
--------------------------------------------------------------------------------

data RestApi = RestApi
  { _charDatabase         :: Snaplet C.CharDatabase
  , _encodingTable        :: Snaplet E.EncodingTable
  }
$(makeLenses ''RestApi)

initRestApi :: Snaplet C.CharDatabase
            -> Snaplet E.EncodingTable
            -> SnapletInit b RestApi
initRestApi cs es = makeSnaplet "rest-api" "JSON 介面" Nothing $ do
  addRoutes [ ("char",          pathArg charHandler)
            , ("chars/all",             allCharsHandler)
            , ("chars/updated",         updatedCharsHandler)
            , ("ref/cns",       pathArg refCnsHandler)
            , ("ref/uni",       pathArg refUniHandler)
            ]
  return $ RestApi cs es

--------------------------------------------------------------------------------
-- Handlers for CharInfo.
--------------------------------------------------------------------------------

frameChar :: CharInfo -> LB.ByteString
frameChar c = toLBS $ object
  [ "version"  .= version
  , "charinfo" .= c
  ]

checkMatchAndFinishWithLBS :: Mime -> LB.ByteString -> Handler b v a
checkMatchAndFinishWithLBS mime output = do
  checkMatch (Just tag)
  finishWithLBS mime output (Just tag)
  where
    tag = etag output

charHandler :: ByteString -> Handler b RestApi ()
charHandler charName = with charDatabase $
  exhaustiveMethodRoutes [ ([GET, HEAD], getter), ([PUT], setter) ]
  -- , ([DELETE], deleter)
  where
    getChar' = fmap frameChar <$> C.getChar charName

    getter = do
      checkExpect
      -------------------------------------------
      output <- errIfNothing 404 =<< getChar'
      -------------------------------------------
      checkMatchAndFinishWithLBS jsonMime output

    setter = do
      checkExpect
      checkContent
      -------------------------------------------
      rq <- readJson
      checkVersion rq
      charInfo <- jsonLookup rq "charinfo"
      -------------------------------------------
      oldtag' <- fmap etag <$> getChar'
      checkMatch oldtag'
      -------------------------------------------
      C.updateChar charName charInfo
      let tag = etag . frameChar $ charInfo
      case oldtag' of
        Nothing -> finishWithCode 201 (Just tag)
        Just _  -> finishWithCode 204 (Just tag)

    {-
    deleter = do
      checkRequest
      -------------------------------------------
      oldtag <- errIfNothing 404 =<< fmap etag <$> getChar'
      -------------------------------------------
      checkMatch (Just oldtag)
      -------------------------------------------
      C.deleteChar charName
      finishWithCode 204 Nothing
    -}

allCharsHandler :: Handler b RestApi ()
allCharsHandler = with charDatabase $
  exhaustiveMethodRoutes [ ([GET, HEAD], getter) ]
  where
    getter = do
      charmap <- C.getChars
      -- FIXME: THIS COULD BE SLOW!
      let etagmap = H.map (etag . frameChar) charmap
      let output = toLBS $ object
            [ "version" .= version
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -------------------------------------------
      -- FIXME: THIS COULD BE SLOW!
      checkMatchAndFinishWithLBS jsonMime output

-- | A data type for 'updatedCharsHandler' to use.
data Diff = Same | Updated LB.ByteString Etag | Deleted deriving Eq

updatedCharsHandler :: Handler b RestApi ()
updatedCharsHandler = with charDatabase $
  exhaustiveMethodRoutes [ ([POST], getter) ]
  where
    diffToMaybeBS (Updated bs _) = Just bs
    diffToMaybeBS Deleted        = Nothing
    diffToMaybeBS Same           = error "The impossible happened!"
    -------------------------------------------
    diffToMaybeEtag (Updated _ tag) = Just tag
    diffToMaybeEtag Deleted         = Nothing
    diffToMaybeEtag Same            = error "The impossible happened!"
    -------------------------------------------
    getter = do
      checkExpect
      -------------------------------------------
      rq <- readJson
      checkVersion rq
      etagmap' <- jsonLookup rq "etagmap"
      -------------------------------------------
      updatemap <- flip H.traverseWithKey etagmap' $ \name cachetag -> do
        output' <- fmap frameChar <$> C.getChar name
        return $ case output' of
          Nothing  -> Deleted
          Just output -> let tag = etag output in
            if tag == cachetag
              then Same
              else Updated output tag
      let diffmap = H.filter (not . (/= Same)) updatemap
      let charmap = H.map diffToMaybeBS   diffmap :: H.HashMap CharName (Maybe LB.ByteString)
      let etagmap = H.map diffToMaybeEtag diffmap :: H.HashMap CharName (Maybe Etag)
      -------------------------------------------
      let output = toLBS $ object
            [ "version" .= version
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -------------------------------------------
      finishWithLBS jsonMime output Nothing

refCnsHandler :: CnsCode -> Handler b RestApi ()
refCnsHandler cnscode = with encodingTable $
  exhaustiveMethodRoutes [ ([GET, HEAD], getter) ]
  where
    getter = do
      unichar <- E.cnsCodeToUniChar cnscode
      let output = toLBS $ object
            [ "version" .= version
            , "uni"     .= unichar
            ]
      finishWithLBS jsonMime output Nothing

refUniHandler :: UniChar -> Handler b RestApi ()
refUniHandler unichar = with encodingTable $
  exhaustiveMethodRoutes [ ([GET, HEAD], getter) ]
  where
    getter = do
      cnscode <- E.uniCharToCnsCode unichar
      blacklisted <- E.isBlackListed unichar
      let output = toLBS $ object
            [ "version"     .= version
            , "cns"         .= cnscode
            , "blacklisted" .= blacklisted
            ]
      finishWithLBS jsonMime output Nothing

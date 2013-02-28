{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import           Data.Maybe
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
import           Mime
import           Version
import           HttpUtil
import qualified CharDatabase as C
import qualified EncodingTable as E

--------------------------------------------------------------------------------
-- JSON tools.
--------------------------------------------------------------------------------

toLBS :: ToJSON a => a -> LB.ByteString
toLBS = encode

fromLBS :: FromJSON a => LB.ByteString -> Handler b v a
fromLBS = errIfNothing 400 . decode'

readJson :: FromJSON a => Handler b v a
readJson = fromLBS =<< readLBS

jsonLookup :: FromJSON a => Object -> Text -> Handler b v a
jsonLookup obj = errIfNothing 400 . parseMaybe (obj .:)

checkVersion :: Object -> Handler b v ()
checkVersion obj = do
  version <- jsonLookup obj "version"
  when (version /= apiVersion) $ err 400

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
            , ("cache/tables",          cacheTablesHandler)
            ]
  return $ RestApi cs es

--------------------------------------------------------------------------------
-- Handlers for CharInfo.
--------------------------------------------------------------------------------

frameChar :: CharInfo -> LB.ByteString
frameChar c = toLBS $ object
  [ "version"  .= apiVersion
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
  routeByMethodWith405 [([GET, HEAD], getter), ([PUT], setter)]
  -- , ([DELETE], deleter)
  where
    getChar' = fmap frameChar <$> C.getChar (fromMaybe "" $ urlDecode charName)

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
      let tag = etag . frameChar $ charInfo
      C.updateChar (fromMaybe "" $ urlDecode charName) charInfo tag
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
  routeByMethodWith405 [([GET, HEAD], getter)]
  where
    getter = do
      charmap <- C.getChars
      -- FIXME: THIS COULD BE SLOW!
      let etagmap = H.map (etag . frameChar) charmap
      let output = toLBS $ object
            [ "version" .= apiVersion
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -------------------------------------------
      -- FIXME: THIS COULD BE SLOW!
      checkMatchAndFinishWithLBS jsonMime output

-- | A data type for 'updatedCharsHandler' to use.
data Change = Updated CharName LB.ByteString Etag | Deleted CharName

updatedCharsHandler :: Handler b RestApi ()
updatedCharsHandler = with charDatabase $
  routeByMethodWith405 [([POST], getter)]
  where
    projInfo :: Change -> (CharName, Maybe LB.ByteString)
    projInfo (Updated name bs _) = (name, Just bs)
    projInfo (Deleted name)      = (name, Nothing)
    -------------------------------------------
    projEtag :: Change -> (CharName, Maybe Etag)
    projEtag (Updated name _ tag) = (name, Just tag)
    projEtag (Deleted name)       = (name, Nothing)
    -------------------------------------------
    getter = do
      checkExpect
      -------------------------------------------
      rq <- readJson
      checkVersion rq
      etagmap' <- jsonLookup rq "etagmap"
      -------------------------------------------
      updatemap <- liftM catMaybes . forM (H.toList etagmap') $ \(name, cachetag) -> do
        output' <- fmap frameChar <$> C.getChar name
        return $ case output' of
          Nothing  -> Just $ Deleted name
          Just output -> let tag = etag output in
            if tag == cachetag
              then Nothing
              else Just $ Updated name output tag
      let charmap = H.fromList . map projInfo $ updatemap
      let etagmap = H.fromList . map projEtag $ updatemap
      -------------------------------------------
      let output = toLBS $ object
            [ "version" .= apiVersion
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -------------------------------------------
      finishWithLBS jsonMime output Nothing

refCnsHandler :: CnsCode -> Handler b RestApi ()
refCnsHandler cnscode = with encodingTable $
  routeByMethodWith405 [([GET, HEAD], getter)]
  where
    getter = do
      unichar <- E.cnsCodeToUniChar cnscode
      let output = toLBS $ object
            [ "version" .= apiVersion
            , "uni"     .= unichar
            ]
      finishWithLBS jsonMime output Nothing

refUniHandler :: UniChar -> Handler b RestApi ()
refUniHandler unichar = with encodingTable $
  routeByMethodWith405 [([GET, HEAD], getter)]
  where
    getter = do
      cnscode <- E.uniCharToCnsCode unichar
      blacklisted <- E.isBlackListed unichar
      let output = toLBS $ object
            [ "version"     .= apiVersion
            , "cns"         .= cnscode
            , "blacklisted" .= blacklisted
            ]
      finishWithLBS jsonMime output Nothing

cacheTablesHandler :: Handler b RestApi ()
cacheTablesHandler = routeByMethodWith405 [([GET, HEAD], getter)]
  where
    getter = do
      chars <- with charDatabase C.getChars
      let cnscodes = mapMaybe (view $ exact.cns) $ H.elems chars
      cns2uni <- forM cnscodes $ with encodingTable . E.cnsCodeToUniChar
      let output = toLBS $ object
            [ "version" .= apiVersion
            , "cns2uni" .= cns2uni
            ]
      finishWithLBS jsonMime output Nothing


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Data.Aeson.TH
import           Control.Monad
import           Control.Lens hiding ((.=))
------------------------------------------------------------------------------
import           Snap
------------------------------------------------------------------------------
import           Type
import           Utils
import qualified CharDatabase as C
import           EncodingTable

------------------------------------------------------------------------------
-- Meta
------------------------------------------------------------------------------

version :: Int
version = 0

------------------------------------------------------------------------------
-- Haskell Datatype <-> JSON
------------------------------------------------------------------------------

$(deriveJSON (drop 1) ''CharDisplay)
$(deriveJSON (drop 1) ''CharInfo)

instance FromJSON CharExact where
   parseJSON (Object v) = CharExact         <$>
                          v .: "cns"        <*>
                          v .: "forced_uni"
   parseJSON _          = mzero

instance ToJSON CharExact where
   toJSON charexact = object
                        [ "cns"        .= view cns       charexact
                        , "forced_uni" .= view forcedUni charexact
                        ]

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
  , _encodingTable        :: Snaplet EncodingTable
  }
$(makeLenses ''RestApi)

initRestApi :: Snaplet C.CharDatabase
            -> Snaplet EncodingTable
            -> SnapletInit b RestApi
initRestApi cs es = makeSnaplet "rest-api" "JSON 介面" Nothing $ do
  addRoutes [ ("char/:uri", withUniqueCapture "uri" charHandler)
            , ("chars/all",                         allCharsHandler)
            ]
  return $ RestApi cs es
  where
    withUniqueCapture :: ByteString -> (ByteString -> Handler b v a) -> Handler b v a
    withUniqueCapture name handler = do
      names <- join <$> maybeToList <$> rqParam name <$> getRequest
      case names of
        [val] -> handler val
        _     -> logError "Capturing failed." >> pass

--------------------------------------------------------------------------------
-- Handlers for CharInfo.
--------------------------------------------------------------------------------

frameChar :: CharInfo -> LB.ByteString
frameChar c = toLBS $ object
  [ "version"  .= version
  , "charinfo" .= c
  ]

charHandler :: ByteString -> Handler b RestApi ()
charHandler charName = with charDatabase $
  methods [GET, HEAD] getter  <|>
  method  PUT         setter  <|>
  -- method  DELETE      deleter <|>
  err405  [GET, HEAD, PUT]
  where
    getChar' = fmap frameChar <$> C.getChar charName

    getter = do
      checkExpect
      -------------------------------------------
      output <- errIfNothing 404 =<< getChar'
      -------------------------------------------
      let tag = etag output
      checkMatch (Just tag)
      -------------------------------------------
      finishWithLBS jsonMime output (Just tag)

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
  methods [GET, HEAD] getter <|>
  err405  [GET, HEAD]
  where
    getter = do
      charmap <- C.getChars
      -- FIXME: THIS MUST BE SLOW!
      let etagmap = H.map (etag . frameChar) charmap
      let output = toLBS $ object
            [ "version" .= version
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -- FIXME: THIS MUST BE SLOW!
      let tag = etag output
      checkMatch (Just tag)
      -------------------------------------------
      finishWithLBS jsonMime output (Just tag)

{-
updatedCharsHandler :: Handler b RestApi ()
updatedCharsHandler = methods [GET, HEAD] . with charDatabase $ do
  cond <- readJson
  writeJson =<< getUpdatedChars undefined

lookupCnsHandler :: ByteString -> Handler b RestApi ()
lookupCnsHandler unicode' = methods [GET, HEAD] . with encodingTable $
  writeJson =<< makeJson <$> lookupCns (decodeUtf8 unicode')
  where makeJson x = object ["unicode" .= x]

lookupUnicodeHandler :: ByteString -> Handler b RestApi ()
lookupUnicodeHandler cns' = methods [GET, HEAD] . with encodingTable $
  writeJson =<< makeJson <$> lookupUnicode (decodeUtf8 cns')
  where makeJson x = object ["cns" .= x]
-}

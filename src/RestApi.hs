{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RestApi
  ( RestApi
  , initRestApi
  ) where

import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Data.Int (Int64)
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.HashMap.Strict as H
import           Blaze.ByteString.Builder
import           Data.Attoparsec
import           Data.Attoparsec.ByteString.Char8
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.Aeson.TH
import           Control.Monad
import           Control.Monad.CatchIO (catch)
import           Control.Lens hiding ((.=))
import           Crypto.Hash
------------------------------------------------------------------------------
import           Snap
import qualified Snap.Iteratee as I
import           Snap.Internal.Parsing
------------------------------------------------------------------------------
import           Type
import qualified CharDatabase as C
import           EncodingTable

------------------------------------------------------------------------------
-- Meta
------------------------------------------------------------------------------

version :: Int
version = 0

maxBodyLen :: Int64
maxBodyLen = 4096

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

------------------------------------------------------------------------------
-- Etag
------------------------------------------------------------------------------

data NormalEtag = StrongNE Etag | WeakNE Etag deriving Eq
data EtagPattern = WildE | NormalE [NormalEtag]

pEtagPattern :: Parser EtagPattern
pEtagPattern = pSpaces *> (pWildPattern <|> pNormalPatterns) <* pSpaces
  where
    pWildPattern = char '*' *> return WildE
    pNormalPatterns = do
      a <- pNormalPattern
      b <- many (pSpaces *> char ',' *> pSpaces *> pNormalPattern)
      return . NormalE $ a:b
    pNormalPattern = choice
      [ StrongNE <$> pQuotedString
      , WeakNE <$> (string "W/" *> pQuotedString)
      ]

etagMatch :: EtagPattern -> Maybe Etag -> Bool
etagMatch _           Nothing  = False
etagMatch WildE       (Just _) = True
etagMatch (NormalE l) (Just t) = StrongNE t `elem` l

etag :: LB.ByteString -> Etag
etag str = digestToHexByteString (hashlazy str :: Digest Skein256_256)

quoteEtag :: Etag -> ByteString
quoteEtag = C8.concatMap quote
  where
    quote '"' = "\""
    quote x   = C8.singleton x

------------------------------------------------------------------------------
-- Basic @finishWith@ and checking
------------------------------------------------------------------------------

err405 :: [Method] -> Handler b v a
err405 allowed = finishWith
  $ setResponseCode 405
  $ setHeader "Allow" allowed'
    emptyResponse
  where
    allowed' = C8.intercalate ", " $ map (C8.pack . show) allowed

finishWithCode :: Int -> Maybe Etag -> Handler b v a
finishWithCode code maybetag = finishWith
  $ setResponseCode code
  $ maybe id (setHeader "Etag" . quoteEtag) maybetag
    emptyResponse

err :: Int -> Handler b v a
err code = finishWithCode code Nothing

errIfNothing :: Int -> Maybe a -> Handler b v a
errIfNothing code = maybe (err code) return

-- | Check HTTP 1.1 request headers.
checkRequest :: Handler b v ()
checkRequest = do
  rq <- getRequest
  when (isJust $ getHeader "Expect" rq) $ err 417

-- | Filter out Content-*.
--   TODO: Check @Content-Type@.
checkPutRequest :: Handler b v ()
checkPutRequest = do
  rq <- getRequest
  when (isJust $ getHeader "Content-Range" rq) $ err 501
  when (isJust $ getHeader "Content-Encoding" rq) $ err 501
  when (isJust $ getHeader "Content-Language" rq) $ err 501
  when (isJust $ getHeader "Content-MD5" rq) $ err 501
  when (isJust $ getHeader "Content-Range" rq) $ err 501

-- | @If-Match@, @If-None-Match@ and @If-Unmodified-Since@
checkMatch :: Maybe Etag -> Handler b v ()
checkMatch tag = do
  rq <- getRequest
  when (isJust $ getHeader "If-Unmodified-Since" rq) $ err 412
  F.forM_ (getHeader "If-Match" rq) $ parseEP >=> ifMatchHandler
  F.forM_ (getHeader "If-None-Match" rq) $ parseEP >=> ifNoneMatchHandler
  where
    parseEP = errIfNothing 400 . maybeResult . parse pEtagPattern
    ifMatchHandler pat = unless (pat `etagMatch` tag) $ err 412
    ifNoneMatchHandler pat = when (pat `etagMatch` tag) $
      methods [GET, HEAD] (finishWithCode 304 tag) <|> err 412

--------------------------------------------------------------------------------
-- JSON and ETag and version checking
--------------------------------------------------------------------------------

toJson' :: ToJSON a => a -> LB.ByteString
toJson' = encode . toJSON

finishWithJson' :: LB.ByteString -> Maybe Etag -> Handler b v c
finishWithJson' str tag = finishWith
  $ setResponseBody body
  $ maybe id (setHeader "ETag" . quoteEtag) tag
  $ setContentType "application/json"
    emptyResponse
  where
    body = I.enumBuilder $ fromLazyByteString str

readJson' :: Handler b v LB.ByteString
readJson' = readRequestBody maxBodyLen
  `catch`
    \(_ :: I.TooManyBytesReadException) -> err 413

fromJson' :: FromJSON a => LB.ByteString -> Handler b v a
fromJson' = errIfNothing 400 . decode'

readJson :: FromJSON a => Handler b v a
readJson = fromJson' =<< readJson'

readJsonMember :: FromJSON a => Object -> Text -> Handler b v a
readJsonMember obj = errIfNothing 400 . parseMaybe (obj .:)

checkVersion :: Object -> Handler b v ()
checkVersion obj = do
  version' <- readJsonMember obj "version"
  when (version' /= version) $ err 400

--------------------------------------------------------------------------------
-- Snaplet state and initializer
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
-- Handlers for CharInfo
--------------------------------------------------------------------------------

frameChar :: CharInfo -> LB.ByteString
frameChar c = toJson' $ object
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
      checkRequest
      -------------------------------------------
      output <- errIfNothing 404 =<< getChar'
      -------------------------------------------
      let tag = etag output
      checkMatch (Just tag)
      -------------------------------------------
      finishWithJson' output (Just tag)

    setter = do
      checkRequest
      checkPutRequest
      -------------------------------------------
      rq <- readJson
      checkVersion rq
      charInfo <- readJsonMember rq "charinfo"
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
      let output = toJson' $ object
            [ "version" .= version
            , "charmap" .= charmap
            , "etagmap" .= etagmap
            ]
      -- FIXME: THIS MUST BE SLOW!
      let tag = etag output
      checkMatch (Just tag)
      -------------------------------------------
      finishWithJson' output (Just tag)

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

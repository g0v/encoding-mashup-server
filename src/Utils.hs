{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module contains some code to deal with HTTP 1.1
that can possibly be used in other REST projects.

-}
module Utils
  (
  -- * HTTP 1.1 tools.
    finishWithCode
  , err
  , errIfNothing
  , implementedMethods
  , methodRoutes
  , checkExpect
  , checkContent
  -- * 'Etag' manipulation and checking.
  , etag
  , checkMatch
  -- * HTTP 1.1 IO.
  , Mime
  , maxBodyLen
  , readLBS
  , finishWithLBS
  ) where

import           Prelude hiding (catch)
------------------------------------------------------------------------------
import           Data.Int (Int64)
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import           Blaze.ByteString.Builder
import           Data.Attoparsec
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as M
import           Control.Monad
import           Control.Monad.CatchIO (catch)
import           Crypto.Hash
------------------------------------------------------------------------------
import           Snap
import qualified Snap.Iteratee as I
import           Snap.Internal.Parsing
------------------------------------------------------------------------------
import           Type

------------------------------------------------------------------------------
-- HTTP 1.1 tools specialized in JSON.
------------------------------------------------------------------------------

-- | Finish with an empty response, along with the code and the @ETag@ (if available).
finishWithCode :: Int -> Maybe Etag -> Handler b v a
finishWithCode code maybetag = finishWith
  $ setResponseCode code
  $ maybe id (setHeader "Etag" . quoteEtag) maybetag
    emptyResponse

-- | A specialized 'finishWithCode' without @ETag@.
err :: Int -> Handler b v a
err code = finishWithCode code Nothing

-- | A convenience function which calls 'err' when the result is 'Nothing'.
errIfNothing :: Int -> Maybe a -> Handler b v a
errIfNothing code = maybe (err code) return

-- | Supported methods in this module.
implementedMethods :: [Method]
implementedMethods = [GET, HEAD, POST, PUT, DELETE]

-- | Method-based routing.
methodRoutes :: [(Method, Handler b v a)] -> Handler b v a
methodRoutes mr = do
  when hasHead $ logError "Assign a handler to HEAD." >> err 500
  when hasUnimplemented $ logError "Assign a handler to an unimplemented method." >> err 500
  m <- rqMethod <$> getRequest
  when (m `notElem` implementedMethods) $ err 501
  let m' = if m == HEAD then GET else m
  fromMaybe err405 $ m' `M.lookup` mr'
  where
    mr' = foldr (uncurry $ M.insertWith (<|>)) M.empty mr
    -------------------------------------------
    allowed = M.keys mr'
    -------------------------------------------
    hasHead = HEAD `elem` allowed
    -------------------------------------------
    hasUnimplemented = any (`notElem` implementedMethods) allowed
    -------------------------------------------
    err405 = finishWith
      $ setResponseCode 405
      $ setHeader "Allow" allowed'
        emptyResponse
      where
        allowed' = C8.intercalate ", " $ map (C8.pack . show) allowed

-- | Check the HTTP 1.1 header @Expect@.
checkExpect :: Handler b v ()
checkExpect = do
  rq <- getRequest
  when (isJust $ getHeader "Expect" rq) $ err 417

-- | Check @Content-*@. This is required for the @PUT@ method.
--   TODO: Take a MIME and check @Content-Type@.
checkContent :: Handler b v ()
checkContent = do
  rq <- getRequest
  when (isJust $ getHeader "Content-Encoding" rq) $ err 501
  when (isJust $ getHeader "Content-Language" rq) $ err 501
  when (isJust $ getHeader "Content-MD5" rq) $ err 501
  when (isJust $ getHeader "Content-Range" rq) $ err 501

------------------------------------------------------------------------------
-- Etag
------------------------------------------------------------------------------

-- | The data type for the content of the headers @If-Match@ and @If-None-Match@.
data NormalEtag = StrongNE Etag | WeakNE Etag deriving Eq
data EtagPattern = WildE | NormalE [NormalEtag]

-- | The parser for the headers @If-Match@ and @If-None-Match@.
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

-- | Matching for @ETags@.
etagMatch :: EtagPattern -> Maybe Etag -> Bool
etagMatch _           Nothing  = False
etagMatch WildE       (Just _) = True
etagMatch (NormalE l) (Just t) = StrongNE t `elem` l

-- | A function to generate @ETag@ for a body.
etag :: LB.ByteString -> Etag
etag str = digestToHexByteString (hashlazy str :: Digest Skein256_256)

-- | A function to quote an @ETag@ according to the standard.
quoteEtag :: Etag -> ByteString
quoteEtag = C8.concatMap quote
  where
    quote '"' = "\""
    quote x   = C8.singleton x

-- | Check @If-Match@, @If-None-Match@ and @If-Unmodified-Since@
--   with respect to the @ETag@ of the resource on the server.
--   'Nothing' is given if the requested resource does not exist.
--   The check is still required in that case because HTTP 1.1 allows
--   wild patterns.
--
--   We always reject @If-Unmodified-Since@ with @501@
--   because we don't use timestamps.
checkMatch :: Maybe Etag -> Handler b v ()
checkMatch tag = do
  rq <- getRequest
  when (isJust $ getHeader "If-Unmodified-Since" rq) $ err 501
  F.forM_ (getHeader "If-Match" rq) $ parseEP >=> ifMatchHandler
  F.forM_ (getHeader "If-None-Match" rq) $ parseEP >=> ifNoneMatchHandler
  where
    parseEP = errIfNothing 400 . maybeResult . parse pEtagPattern
    ifMatchHandler pat = unless (pat `etagMatch` tag) $ err 412
    ifNoneMatchHandler pat = when (pat `etagMatch` tag) $
      methods [GET, HEAD] (finishWithCode 304 tag) <|> err 412

------------------------------------------------------------------------------
-- HTTP 1.1 IO specilized in JSON.
------------------------------------------------------------------------------

-- | The type of a base MIME. Subjects to changes.
type Mime = ByteString

-- | Maximum length for the request body.
maxBodyLen :: Int64
maxBodyLen = 4096

-- | Read the HTTP request up to 'maxBodyLen'.
readLBS :: Handler b v LB.ByteString
readLBS = readRequestBody maxBodyLen
  `catch`
    \(_ :: I.TooManyBytesReadException) -> err 413

-- | Finish with the given body immediately.
finishWithLBS :: Mime -> LB.ByteString -> Maybe Etag -> Handler b v c
finishWithLBS mime str tag = finishWith
  $ setResponseBody body
  $ maybe id (setHeader "ETag" . quoteEtag) tag
  $ setContentType mime
    emptyResponse
  where
    body = I.enumBuilder $ fromLazyByteString str

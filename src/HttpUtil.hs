{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |

This module contains some code to deal with HTTP 1.1
that can possibly be used in other REST projects.

-}
module HttpUtil
  (
  -- * HTTP 1.1 tools.
    finishWithCode
  , err
  , errIfNothing
  , supportedMethods
  , routeByMethodWith405
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

import           Data.Int (Int64)
import           Data.List
import qualified Data.Foldable as F
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import           Blaze.ByteString.Builder
import           Data.Attoparsec
import           Data.Attoparsec.ByteString.Char8
import           Control.Monad
import qualified Control.Monad.CatchIO as MIO (catch)
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
supportedMethods :: [Method]
supportedMethods = [GET, HEAD, POST, PUT, DELETE]

-- | Method-based routing. The mapping needs to be exhaustive so that
--   a proper HTTP @405@ error message can be generated,
--   where methods without handlers are assumed to be disallowed.
--   Multiple handlers for the same method are joined with '<|>'.
--   HTTP error @501@ is generated for any method not in 'supportedMethods'.
--
--   Assigning a handler to an unsupported method has no effect
--   (except increasing the computation time).
routeByMethodWith405 :: [([Method], Handler b v a)] -> Handler b v a
routeByMethodWith405 r = do
  m <- rqMethod <$> getRequest
  when (m `notElem` supportedMethods) $ err 501
  let handlers = matchedHandlers m
  when (null handlers) err405
  foldl1 (<|>) handlers
  where
    matchedHandlers m = map snd $ filter (elem m . fst) r
    -------------------------------------------
    err405 = finishWith
      $ setResponseCode 405
      $ setHeader "Allow" assigned'
        emptyResponse
      where
        assigned = nub $ concatMap fst r
        assigned' = C8.intercalate ", " $ map (C8.pack . show) assigned

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
--   because we don't use timestamps. The HTTP 1.1 standard
--   is not crystal clear on the proper error code, but we think
--   this is a more reasonable choice than the @412@ error code
--   when the timestamps are intentionally avoided.
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
  `MIO.catch`
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

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.Maybe
import System.Environment
import Data.Aeson
import Data.ByteString.Char8 as B
import Data.ByteString.Lazy as LB
import Data.Text.Encoding
import Network.HTTP
import Network.URI

import System.Console.CmdLib as C
import Control.Monad

import Type
import Mime
import Version
import DataImporter.Google0

clientVersion :: Int64
clientVersion = 0

data Config = Config { format :: String, file :: FilePath, url :: String }
  deriving (Typeable, Data, Eq)

instance Attributes Config where
  attributes _ = C.group "Options"
    [ format %> [ Help "Input file format"
                , Default ("google0" :: String)
                ]
    , file   %> [ Help "Input file"
                , Required True
                ]
    , url    %> [ Help "REST API url"
                , Required True
                ]
    ]

instance RecordCommand Config where
  mode_summary _ = "Data importer."

putCharInfo :: String -> CharName -> CharInfo -> IO ()
putCharInfo basestr charname info = do
  let output = LB.toStrict . encode $ object
        [ "version"  .= apiVersion
        , "charinfo" .= info
        ]
  let rq = setRequestBody' (jsonMime, output) $
           mkRequest PUT fullpath :: Request B.ByteString
  print (charname, info)
  rslt <- simpleHTTP rq
  case rslt of
    Left msg -> fail $ show msg
    Right res  -> print (fullpath, rspCode res)

  where
    fullpath =
      case (path', base') of
        (Just path, Just base) -> path `relativeTo` base
        _                      -> error "Invalid URL"
      where
        path' = parseRelativeReference $
                escapeURIString isUnreserved $
                B.unpack charname
        base' = parseAbsoluteURI basestr

    setRequestBody' :: (B.ByteString, B.ByteString) -> Request B.ByteString -> Request B.ByteString
    setRequestBody' (typ, body) req = req' { rqBody = body }
      where
        req' = replaceHeader HdrContentType (B.unpack typ) $
               replaceHeader HdrContentLength (show $ B.length body) $
               req

parse :: String -> LB.ByteString -> Maybe [(CharName, CharInfo)]
parse format str =
  case format of
    "google0" -> parseGoogle0 str
    _         -> Nothing

dispatcher :: Config -> IO ()
dispatcher flags = do
  content <- LB.readFile (file flags)
  csv <- maybe (fail "Parsing failed.") return $ parse (format flags) content
  forM_ csv . uncurry $ putCharInfo (url flags)

main :: IO ()
main = getArgs >>= executeR Config {} >>= dispatcher

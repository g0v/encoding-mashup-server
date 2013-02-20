module Utils
  ( getResourceDir
  ) where

import System.FilePath
import Control.Applicative
import Paths_encoding_mashup_server

getResourceDir :: FilePath -> IO FilePath
getResourceDir name = (</> "resources" </> name) <$> getDataDir

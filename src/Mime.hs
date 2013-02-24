{-# LANGUAGE OverloadedStrings #-}
module Mime where

import Type

jsonMime :: Mime
jsonMime = "application/json"
{-# INLINE jsonMime #-}

cssMime :: Mime
cssMime = "text/css"
{-# INLINE cssMime #-}

jsMime :: Mime
jsMime = "application/javascript"
{-# INLINE jsMime #-}

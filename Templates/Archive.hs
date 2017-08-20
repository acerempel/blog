{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Templates.Archive where

import Text.Blaze.Html5 as Html hiding ( meta )
import Text.Blaze.Html5.Attributes ( class_, datetime, href )

import qualified Content
import qualified Page
import Types
import Utils

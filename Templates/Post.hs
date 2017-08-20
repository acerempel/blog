{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns   #-}
module Templates.Post where

import Data.Time.Calendar ( showGregorian )
import Data.Time.Format
import Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes ( class_, datetime, href )

import Page.Meta as Page
import Types
import Utils

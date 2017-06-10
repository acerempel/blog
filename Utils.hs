{-# LANGUAGE ViewPatterns #-}
module Utils
    ( fromMaybe
    , maybe
    , showGregorian
    , showText
    , whenMaybe
    , year
    , (<>)
    ) where

import Data.Maybe ( maybe, fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text as Text
import Data.Time.Calendar ( Day, showGregorian, toGregorian )

showText :: Show a => a -> Text
showText = Text.pack . show

whenMaybe :: Monoid m => Maybe a -> (a -> m) -> m
whenMaybe =
    flip (maybe mempty)

year :: Day -> Integer
year (toGregorian -> (y, _, _)) = y

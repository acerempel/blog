{-# LANGUAGE ViewPatterns #-}
module Utils
    ( Day
    , catMaybes
    , fromMaybe
    , maybe
    , showGregorian
    , (<>)
    ) where

import Data.Maybe ( catMaybes, maybe, fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text as Text
import Data.Time.Calendar ( Day, showGregorian )

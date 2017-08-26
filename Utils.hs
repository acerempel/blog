{-# LANGUAGE ViewPatterns #-}
module Utils
    ( fromMaybe
    , maybe
    , showGregorian
    , showText
    , whenJust
    , year
    , (<>)
    ) where

import Data.Maybe ( maybe, fromMaybe )
import Data.Monoid ( (<>) )
import Data.Text as Text
import Data.Time.Calendar ( Day, showGregorian, toGregorian )

showText :: Show a => a -> Text
showText = Text.pack . show

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust =
    flip (maybe (return ()))

year :: Day -> Integer
year (toGregorian -> (y, _, _)) = y

{-# LANGUAGE FunctionalDependencies, GADTs #-}
module Properties
  ( HasDate(..), HasTitle(..), HasPageTitle(..)
  , HasPreview(..), HasIncipit(..), HasContent(..)
  , Parse(..), render
  ) where

import Introit

import Data.Time.Calendar ( Day )
import qualified Lucid
import Text.MMark ( MMark )
import qualified Text.MMark as MMark

class HasDate a where
  date :: a -> Day

class HasContent t => HasTitle a t | a -> t where
  title :: a -> Maybe t

class HasPageTitle a where
  titleForPage :: a -> Text

class HasPreview a where
  -- | 'Nothing' signifies that the prose is short enough that the preview
  -- would just be the whole prose, so is redundant.
  preview :: a -> Maybe MMark

class HasIncipit a where
  incipit :: a -> Text

class HasContent a where
  content :: a -> MMark

instance HasContent MMark where
  content = id

render :: HasContent a => a -> Lucid.Html ()
render = MMark.render . content

class Parse a where
  parse :: FilePath -> Text -> Either String a

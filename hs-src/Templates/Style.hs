{-# LANGUAGE GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Templates.Style
  ( CSS, propertyDeclaration, style_
  , variableMultiple, lineHeightMultiple
  , variable
  , colour, lineHeight, fontSize
  ) where

import Introit
import Numeric
import qualified Text

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Lucid

newtype CSS = CSS (Seq Text)
  deriving newtype ( Semigroup, Monoid )

style_ (CSS csses) = Lucid.style_ (fold csses)

propertyDeclaration property value =
  CSS $ Seq.singleton (property <> ": " <> value <> ";")

variableMultiple :: Text -> Text -> Double -> CSS
variableMultiple varName property multiplier =
  propertyDeclaration property $
  "calc(" <> variable varName <> " * " <> Text.pack (show multiplier) <> ")"

variable varName =
  "var(--" <> varName <> ")"

colour colourName =
  propertyDeclaration "color" (variable ("text-colour-" <> colourName))

lineHeightMultiple = variableMultiple "base-line-height"

lineHeight = lineHeightMultiple "line-height"

fontSize = variableMultiple "base-font-size" "font-size"

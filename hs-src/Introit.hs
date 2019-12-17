module Introit ( module X, Text, foldrMapM ) where

import Control.Applicative as X
import Control.Monad as X
import Data.Bifunctor as X
import Data.Either as X
import Data.Foldable as X
import Data.Function as X
import Data.List as X
import Data.Maybe as X
import Data.Monoid as X
import Data.Ord as X
import Data.Text ( Text )
import Data.Traversable as X
import System.IO as X

foldrMapM :: (Foldable f, Monoid z, Monad m) => (a -> m z) -> f a -> m z
foldrMapM f =
   foldlM (\z2 z1 -> (<>) <$> return z2 <*> f z1) mempty

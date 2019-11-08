module List.Base
  ( module Data.List
  , List, empty, singleton, fromList
  ) where

import Data.List

type List = []

empty :: List a
empty = []

singleton :: a -> List a
singleton a = [a]

fromList :: [a] -> List a
fromList = id

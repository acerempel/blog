{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Page ( Page(..), pageToHtml ) where

import Data.Text ( Text )
import Text.Blaze.Html ( Html )

import Page.Meta
import qualified Templates.Archive as Templates
import qualified Templates.Home as Templates
import qualified Templates.Post as Templates
import Types

pageToHtml :: [Post] -> Page -> Html
pageToHtml posts page =
    case page of
        Home ->
            Templates.home pageMeta posts
        Archive ->
            Templates.archive pageMeta posts
        Post post ->
            Templates.post pageMeta post
  where
    pageMeta =
        meta page

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

pageToHtml :: Page -> Html
pageToHtml page =
    case page of
        Home posts ->
            Templates.home pageMeta posts
        Archive posts ->
            Templates.archive pageMeta posts
        Post post ->
            Templates.post pageMeta post
  where
    pageMeta =
        meta page

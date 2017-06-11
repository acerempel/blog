{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Page.Meta ( Meta(..), meta ) where

import Data.Text ( Text )

import Types
import Utils

meta :: Page -> Meta
meta (Home _) = M
    { url = "/"
    , pageTitle = Nothing
    , linkTitle = "Recent"
    }
meta (Archive _) = M
    { url = "/archive.html"
    , pageTitle = Just "Posts"
    , linkTitle = "Archive"
    }
meta (Post P{date, identifier, postTitle}) = M
    { url = "/posts/" <> identifier <> ".html"
    , pageTitle = postTitle
    , linkTitle = fromMaybe identifier postTitle
    }

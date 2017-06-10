{-# LANGUAGE OverloadedStrings
           , NamedFieldPuns    #-}
module Page.Meta ( Meta(..), meta ) where

import Data.Text ( Text )

import Types
import Utils

meta :: Page -> Meta
meta Home = M
    { url = "/"
    , pageTitle = Nothing
    , linkTitle = "Recent"
    }
meta Archive = M
    { url = "/archive"
    , pageTitle = Just "Posts"
    , linkTitle = "Archive"
    }
meta (Post P{date, identifier, postTitle}) = M
    { url = "/posts/" <> showText (year date) <> "/" <> identifier
    , pageTitle = postTitle
    , linkTitle = fromMaybe identifier postTitle
    }

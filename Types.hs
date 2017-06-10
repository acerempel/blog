module Types where

import Data.Text ( Text )
import Data.Time.Calendar ( Day )
import Text.Blaze.Html ( Html )
import Text.Pandoc ()


data Page = Home
          | Archive
          | Post Post

data Meta = M
    { url :: Text
    , pageTitle :: Maybe Text
    , linkTitle :: Text
    }

data Post = P
    { content :: Html
    , identifier :: PostID
    , date :: Day
    , postTitle :: Maybe Text
    , isDraft :: Bool
    }

type PostID = Text

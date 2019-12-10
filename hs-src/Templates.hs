module Templates ( Html
                 , PageContent
                 , archive, post, home, tagsList
                 , page
                 , IncludeTags ) where

import Introit hiding ( for_ ) -- I use 'Lucid.for_', meaning the HTML5 attribute, below
import qualified Text

import Control.Monad ( when )
import Control.Monad.Trans.Class ( lift )
import Data.Time.Calendar ( Day, showGregorian )
import Data.Time.Format
import Lucid hiding ( Html )

import Post

type IncludeTags = Bool

home :: Post -> [Post] -> [Post] -> PageContent
home hello misc posts =
  let mainContent includeTags = do
        article_ [] (body hello)
        hr_ []
        section_ do
          h1_ "Recent posts"
          foldrMapM (archiveEntry includeTags) posts -- TODO!
          p_ $ a_ [ href_ "/posts" ] "See all posts …"
        hr_ []
        section_ do
          h1_ "Miscellaneous"
          p_ $ a_ [ href_ "/neat_links" ] "Links I like"
      pageDescription = Just "A very mysterious website …"
      pageTitle = "Hello"
  in PageContent{..}

archive :: [Post] -> PageContent
archive posts =
  let mainContent includeTags =
        foldrMapM (archiveEntry includeTags) posts
      pageDescription =
        Just "Posts both fictional and non-fictional on a variety of topics."
  in PageContent
       { mainContent
       , pageDescription
       , pageTitle = "All Posts"}

post :: Post -> PageContent
post Post{..} =
  let
    mainContent includeTags =
      article_ [ class_ "post full" ] do
        header_ do
          maybe ((lift . Left) (MissingField url "date")) date published
          whenMaybe title \theTitle ->
            h2_ [ class_ "title" ] (link url pageTitle theTitle)
        body
        when includeTags $ footer_ $
          p_ (tagLinks tags)
  in PageContent
      { mainContent
      , pageDescription = description
      , pageTitle }

tagsList :: [(Tag, Int)] -> Html
tagsList tagsWithCounts = do
    h1_ "Tags"
    p_ $ ul_ $
      foldMap tagWithCount tagsWithCounts
  where
    tagWithCount :: (Tag, Int) -> Html
    tagWithCount (tag, count) =
      li_ $ p_ do
        tagLink tag
        " (" <> toHtml (show count) <> " posts)"


archiveEntry :: IncludeTags -> Post -> Html
archiveEntry includeTags Post{..} =
   article_ [ class_ ("post " <> if showPreview then "full" else "summary") ] do
      maybe ((lift . Left) (MissingField url "date")) date published
      whenMaybe title \theTitle ->
        h2_ [ class_ "title" ] (link url pageTitle theTitle)
      whenMaybe mSynopsis \synopsis ->
        -- TODO make synopsis MMark
        p_ [ class_ "synopsis" ] synopsis
      when showPreview do
        case preview of
          Just thePreview -> do
            thePreview
            p_ (a_ [ href_ url, title_ pageTitle ] "Continue reading …")
          Nothing ->
            body
      when includeTags $
         p_ (tagLinks tags)
  where
    showPreview =
      isNothing title || isNothing mSynopsis

date :: Day -> Html
date theDate =
   div_ $ time_
       [ datetime_ ((Text.pack . showGregorian) theDate) ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

tagLinks :: [Tag] -> Html
tagLinks [] = mempty
tagLinks theTags =
  "Tagged as " <> mconcat (intersperse ", " (map tagLink theTags))

tagLink :: Tag -> Html
tagLink tagName =
  a_ [ href_ ("/tags/" <> tagName) ] $ toHtml tagName

link :: Text -> Text -> Html -> Html
link url title text =
  -- TODO: put title attr back -- it needs to be turned into plain text
  a_ [ href_ url, title_ title ] $ text

data PageContent = PageContent
    { mainContent :: IncludeTags -> Html
    , pageDescription :: Maybe Text
    , pageTitle :: Text }

page :: IncludeTags -> PageContent -> Html
page includeTags PageContent{mainContent, pageDescription, pageTitle} = do
    doctype_
    html_ [ lang_ "en" ] do
        head_ do
            meta_
                [ httpEquiv_ "ContentType"
                , content_ "text/html; charset=utf-8" ]
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1" ]
            meta_
                [ charset_ "utf-8" ]
            title_ $ toHtml $ pageTitle <> " … ‹three dots›"
            whenMaybe pageDescription \description ->
              meta_ [ name_ "description", content_ description ]
            link_ [ rel_ "stylesheet" , href_ "/styles.css" ]
            script_
              [ src_ "/scripts/colour-scheme.js"
              , defer_ "" {- re. defer_: I wish boolean attributes in Lucid could be written without the argument -} ]
              ("" :: String) {- Also, it would be nice if this argument were optional for script_ -}
        body_ [ class_ "colour-scheme-auto" ] do
          div_ [ class_ "container" ] do
            header_ do
                h1_ [ class_ "slightly-bigger semibold" ] $ a_ [ href_ "/" ] "Three dots …"
            main_ (mainContent includeTags)
            footer_ do
              settings

settings :: Html
settings =
  section_ do
    h2_ [ class_ "semibold" ] "Appearance"
    p_ do
      label_ [ for_ "colour-scheme-select" ] "Colour scheme:"
      select_ [ id_ "colour-scheme-select", required_ "" {- See above re. defer_ -} ] do
        option_ [ value_ "auto", selected_ "" {- See above re. defer_ -} ] "System setting"
        option_ [ value_ "light" ] "Light"
        option_ [ value_ "dark" ] "Dark"

whenMaybe :: Monoid f => Maybe a -> (a -> f) -> f
whenMaybe mThing f =
  maybe mempty f mThing

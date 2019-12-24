module Templates ( Html
                 , PageContent
                 , archive, post, home, aboutPage , tagsList
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

home :: Post -> [Post] -> PageContent
home hi posts =
  let mainContent :: IncludeTags -> Html
      mainContent includeTags = do
        article_ do
          h1_ headingClasses "Hello!"
          body hi
        section_ [ class_ "mt-1-1" ] do
          h1_ headingClasses "Recent posts"
          foldrMapM (archiveEntry includeTags) posts -- TODO!
          p_ [ class_ "link-plain fs-1-1 mt-1-1" ] $ a_ [ href_ "/posts" ] "See all posts …"
      pageDescription = Just "A very mysterious website …"
      pageTitle = "Hello"
      headingClasses = [ class_ "bold fs-9-8 mb-3-8" ]
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
      article_ [ class_ "full" ] do
        header_ do
          maybe ((lift . Left) (MissingField url "date")) date published
          whenMaybe title articleFullTitle
        body
        when includeTags $ footer_ $
          p_ (tagLinks tags)
  in PageContent
      { mainContent
      , pageDescription = description
      , pageTitle }

articleFullTitle html =
  h1_ [ class_ "title bold mb-1-2 fs-3-2 lh-3-2" ] html

aboutPage :: Post -> PageContent
aboutPage Post{..} =
  let
    content =
      article_ [ class_ "full" ] do
        whenMaybe title (header_ . articleFullTitle)
        body
  in
    PageContent
      { mainContent = const content
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
   article_ [ class_ ("post mb-1-1 fs-1-1 " <> if showPreview then "preview" else "summary") ] do
      maybe ((lift . Left) (MissingField url "date")) date published
      whenMaybe title \theTitle ->
        h2_ [ class_ "semibold fs-6-5 lh-6-5" ] (link url pageTitle theTitle)
      whenMaybe synopsis \theSynopsis ->
        p_ [ class_ "synopsis fs-1-1 lh-1-1 oblique" ] theSynopsis
      when showPreview do
        case preview of
          Just thePreview -> do
            thePreview
            p_ [ class_ "medium oblique link-plain" ] (link url pageTitle "Continue reading …")
          Nothing ->
            body
      when includeTags $
         p_ (tagLinks tags)
  where
    showPreview =
      isNothing title || isNothing synopsis

date :: Day -> Html
date theDate =
   time_
       [ datetime_ ((Text.pack . showGregorian) theDate)
       , class_ "date block light oblique colour-lighter fs-1-1 lh-1-1" ]
       $ toHtml (formatTime defaultTimeLocale "%d %B %Y" theDate)

tagLinks :: [Tag] -> Html
tagLinks [] = mempty
tagLinks theTags =
  "Tagged as " <> mconcat (intersperse ", " (map tagLink theTags))

tagLink :: Tag -> Html
tagLink tagName =
  a_ [ href_ ("/tags/" <> tagName) ] $ toHtml tagName

link :: URL -> Text -> Html -> Html
link (URL url) title text =
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
            main_ [ class_ "mt-2-3 mb-1-1" ] (mainContent includeTags)
            footer_ [ class_ "mb-1-1" ] do
              navigation
              settings

settings :: Html
settings =
  section_ do
    h2_ footerHeadingClasses "Appearance"
    p_ footerParagraphClasses do
      label_ [ for_ "colour-scheme-select" ] "Colour scheme:"
      select_ [ id_ "colour-scheme-select", required_ "" {- See above re. defer_ -} ] do
        option_ [ value_ "auto", selected_ "" {- See above re. defer_ -} ] "System setting"
        option_ [ value_ "light" ] "Light"
        option_ [ value_ "dark" ] "Dark"

navigation :: Html
navigation =
  section_ [ class_ "mb-3-4 mr-1-1" ] do
    h2_ footerHeadingClasses "Navigation"
    nav_ [ class_ "link-plain" ] do
      traverse_ footerLink
        [("Home", "/"), ("Posts", "/posts"), ("About", "/introduction")]
  where
    footerLink :: (Html, Text) -> Html
    footerLink (text, url) =
      p_ footerParagraphClasses (a_ [ href_ url ] text)

footerHeadingClasses =
  [ class_ "semibold fs-1-1 lh-3-4" ]

footerParagraphClasses =
  [ class_ "fs-1-1 lh-3-4 mt-1-3" ]

whenMaybe :: Monoid f => Maybe a -> (a -> f) -> f
whenMaybe mThing f =
  maybe mempty f mThing

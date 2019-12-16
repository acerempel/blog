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

home :: [Post] -> [Post] -> PageContent
home abouts posts =
  let mainContent :: IncludeTags -> Html
      mainContent includeTags = do
        section_ do
          h1_ [ class_ "bold font-size-one margin-bottom-one-half" ] "About"
          traverse_ aboutLink abouts
        section_ do
          h1_ [ class_ "bold font-size-one margin-top-one margin-bottom-one-half" ] "Recent posts"
          foldrMapM (archiveEntry includeTags) posts -- TODO!
          p_ [ class_ "further" ] $ a_ [ href_ "/posts" ] "See all posts …"
      pageDescription = Just "A very mysterious website …"
      pageTitle = "Hello"
  in PageContent{..}
  where
    aboutLink = \about ->
      p_ [ class_ "margin-bottom-one-half" ]$
        a_ [ href_ (fromURL (url about)) ] $
          toHtml (Post.pageTitle about)

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
          whenMaybe title \theTitle ->
            h1_ [ class_ "title" ] theTitle
        body
        when includeTags $ footer_ $
          p_ (tagLinks tags)
  in PageContent
      { mainContent
      , pageDescription = description
      , pageTitle }

aboutPage :: Post -> PageContent
aboutPage Post{..} =
  let
    content =
      article_ [ class_ "full" ] do
        whenMaybe title \theTitle ->
          header_ do
            h1_ [ class_ "title" ] theTitle
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
   article_ [ class_ ("post " <> if showPreview then "preview" else "summary") ] do
      maybe ((lift . Left) (MissingField url "date")) date published
      whenMaybe title \theTitle ->
        h2_ [ class_ "title" ] (link url pageTitle theTitle)
      whenMaybe synopsis \theSynopsis ->
        -- TODO make synopsis MMark
        p_ [ class_ "synopsis" ] theSynopsis
      when showPreview do
        case preview of
          Just thePreview -> do
            thePreview
            p_ [ class_ "further" ] (link url pageTitle "Continue reading …")
          Nothing ->
            body
      when includeTags $
         p_ (tagLinks tags)
  where
    showPreview =
      isNothing title || isNothing synopsis

date :: Day -> Html
date theDate =
   div_ $ time_
       [ datetime_ ((Text.pack . showGregorian) theDate), class_ "date" ]
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
            main_ (mainContent includeTags)
            footer_ [ class_ "margin-bottom-one" ] do
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
  section_ [ class_ "margin-bottom-three-quarters margin-right-one" ] do
    h2_ footerHeadingClasses "Navigation"
    nav_ do
      traverse_ footerLink
        [("Home", "/"), ("Posts", "/posts"), ("About", "/introduction")]
  where
    footerLink :: (Html, Text) -> Html
    footerLink (text, url) =
      p_ footerParagraphClasses (a_ [ href_ url ] text)

footerHeadingClasses =
  [ class_ "semibold font-size-seven-eighths line-height-three-quarters" ]

footerParagraphClasses =
  [ class_ "font-size-seven-eighths line-height-three-quarters" ]

whenMaybe :: Monoid f => Maybe a -> (a -> f) -> f
whenMaybe mThing f =
  maybe mempty f mThing

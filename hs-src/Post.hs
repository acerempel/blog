module Post ( Post(..), readPostFromFile ) where

import Introit
import qualified Text
import Utilities

import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:) )
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.FilePath
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark


data Post = Post
   { slug :: Text -- ^ Identifier to use for the slug in the url.
   , title :: Text -- ^ Title.
   , content :: MMark -- ^ The post body.
   , synopsis :: Text -- ^ A little description or summary or teaser.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   }


readPostFromFile :: FilePath -- ^ Path to the post (/including/ the postsDir).
                 -> Action Post
readPostFromFile filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    either (throwFileError filepath) return $ do
      body <-
         first (MMark.parseErrorsPretty contents) $
         second (MMark.useExtension hyphensToDashes) $
         MMark.parse filepath contents
      yaml <- maybe (Left noMetadataError) Right $
         MMark.projectYaml body
      withMetadata body yaml
 where
   withMetadata content = Yaml.parseEither $
      Yaml.withObject "metadata" $ \metadata -> do
         title    <- metadata .: "title"
         date     <- metadata .: "date"
         synopsis <- metadata .: "synopsis"
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = (Text.pack . takeBaseName) filepath
         return Post
            { title
            , synopsis
            , slug
            , composed
            -- TODO: Distinguish these --- maybe.
            , published = composed
            , isDraft = False
            , content }

   hyphensToDashes :: MMark.Extension
   hyphensToDashes = MMark.inlineTrans $
      MMark.mapInlineRecursively $
      MMark.mapInlineText $
      Text.replace "--" "–" .
      Text.replace "---" "—"

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"

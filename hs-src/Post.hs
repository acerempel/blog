{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Post ( Post(..), Tag, readPost ) where

import Introit
import qualified Text
import Utilities

import System.FilePath
import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:), (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import qualified Network.URI.Encode as URI
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension as MMark


data Post = Post
   { slug :: String -- ^ Identifier to use for the slug in the url.
                    -- TODO: Should this really be the 'Routes.Post'?
   , title :: Text -- ^ Title.
   , content :: MMark -- ^ The post body.
   , synopsis :: Text -- ^ A little description or summary or teaser.
   , composed :: Day -- ^ Date of composition.
   , published :: Day -- ^ Date of publication.
   , isDraft :: Bool -- ^ Whether this post is a draft or is published.
   , tags :: [Tag] -- ^ Some tags.
   }

type Tag = Text

readPost :: FilePath
         -> IO Post
readPost filepath = do
    contents <- Text.readFile filepath
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
         tags     <- metadata .:? "tags" .!= []
         composed <- parseTimeM True defaultTimeLocale dateFormat date
         let slug = URI.encode $ takeBaseName filepath
         return Post
            { published = composed -- TODO: Distinguish these --- maybe.
            , isDraft = False
            , .. }

   hyphensToDashes :: MMark.Extension
   hyphensToDashes = MMark.inlineTrans $
      MMark.mapInlineRecursively $
      MMark.mapInlineText $
      Text.replace "--" "–" .
      Text.replace "---" "—"

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"

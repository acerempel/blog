{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Post ( Post(..), Tag, readPost ) where

import Introit
import qualified Text
import Utilities

import Data.Time.Calendar ( Day )
import Data.Time.Format ( parseTimeM, defaultTimeLocale )
import Data.Yaml ( (.:), (.:?), (.!=) )
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.FilePath
import qualified Network.URI.Encode as URI
import qualified Text.Megaparsec as MP
import Text.MMark ( MMark )
import qualified Text.MMark as MMark
import Text.MMark.Extension.PunctuationPrettifier


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
         -> Action Post
readPost filepath = do
    need [filepath]
    contents <- liftIO $ Text.readFile filepath
    either (throwFileError filepath) return $ do
      body <-
         first MP.errorBundlePretty $
         second (MMark.useExtension punctuationPrettifier) $
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

   noMetadataError =
      "Couldn't find a metadata block in file " <> filepath

   dateFormat = "%e %B %Y"

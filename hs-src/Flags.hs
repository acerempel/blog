module Flags ( Flag, handle, options, defaultOptions ) where

import System.Console.GetOpt
import Build


type Flag = Build.Options -> Build.Options

handle :: [Flag] -> Build.Options
handle flags =
   -- We flip (.) in order to compose flags from left to right, so that ---
   -- as you would expect --- rightmore cli flags override leftmore ones.
   foldr (flip (.)) id flags defaultOptions

options :: [OptDescr (Either String Flag)]
options = fmap Right <$>
   [ configFileOption
   , liveModeOption
   , devModeOption
   , buildDirOption
   , postsDirOption
   , draftsDirOption
   , stylesDirOption
   , imagesDirOption
   , draftsOption
   , noDraftsOption
   , tagsOption
   , noTagsOption ]

defaultOptions :: Options
defaultOptions = Options
   { buildDir = "_site"
   , postsDir = "posts"
   , draftsDir = "drafts"
   , stylesDir = "styles"
   , imagesDir = "images"
   , siteConfigFile = "config"
   , includeDrafts = False
   , includeTags = False }


liveModeOption =
   Option [] ["live", "production", "deployment"]
      (NoArg ( useConfigFile "config.production"
             . useBuildDir "_site.production"
             . don'tIncludeDrafts ))
      "Build the site as though the result is to be deployed."

devModeOption =
   Option [] ["local", "development"]
      (NoArg ( useConfigFile "config.local"
             . useBuildDir "_site.local" ))
      "Build the site for a local development server."

configFileOption =
   Option [] ["config", "config-file"]
      (ReqArg useConfigFile "FILE")
      "Get the site configuration from this file (relative to cwd)."

buildDirOption =
   Option [] ["builddir"]
      (ReqArg useBuildDir "DIRECTORY")
      "Where to place the generated site. (Default: _site)"

postsDirOption =
   Option [] ["postsdir"]
      (ReqArg usePostsDir "DIRECTORY")
      "Where to look for posts. (Default: posts)"

draftsDirOption =
   Option [] ["draftsdir"]
      (ReqArg useDraftsDir "DIRECTORY")
      "Where to look for drafts. (Default: drafts)"

stylesDirOption =
   Option [] ["stylesdir"]
      (ReqArg useStylesDir "DIRECTORY")
      "Where to look for stylesheets. (Default: styles)"

imagesDirOption =
   Option [] ["imagesdir"]
      (ReqArg useImagesDir "DIRECTORY")
      "Where to look for images. (Default: images)"

draftsOption =
   Option [] ["drafts"]
      (NoArg doIncludeDrafts)
      "Build drafts along with published posts."

noDraftsOption =
   Option [] ["no-drafts"]
      (NoArg don'tIncludeDrafts)
      "Don't build drafts. (This is the default.)"

tagsOption =
   Option [] ["tags"]
      (NoArg doIncludeTags)
      "Build tags."

noTagsOption =
   Option [] ["no-tags"]
      (NoArg don'tIncludeTags)
      "Don't build tags. (This is the default.)"

useConfigFile :: FilePath -> Flag
useConfigFile siteConfigFile configuration =
   configuration{ siteConfigFile }

useBuildDir, usePostsDir, useDraftsDir, useStylesDir, useImagesDir :: FilePath -> Flag

useBuildDir buildDir configuration =
   configuration{ buildDir }

usePostsDir postsDir configuration =
   configuration{ postsDir }

useDraftsDir draftsDir configuration =
   configuration{ draftsDir }

useStylesDir stylesDir configuration =
   configuration{ stylesDir }

useImagesDir imagesDir configuration =
   configuration{ imagesDir }

doIncludeDrafts :: Flag
doIncludeDrafts configuration =
   configuration{ includeDrafts = True }

don'tIncludeDrafts :: Flag
don'tIncludeDrafts configuration =
   configuration{ includeDrafts = False }

doIncludeTags :: Flag
doIncludeTags configuration =
   configuration{ includeTags = True }

don'tIncludeTags :: Flag
don'tIncludeTags configuration =
   configuration{ includeTags = False }

module BuildV2 where

import Introit

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import System.Directory
    ( listDirectory, doesDirectoryExist, doesFileExist )
import System.FilePath
    ( (</>), takeDirectory, takeExtension )

import Lucid ( Html )
import qualified Lucid as Html

import Post ( Post(..), readPost )
import Routes ( Route(..), targetFile, ContentType(..) )
import qualified Templates

data Options = Options
    { postsSubDirectory :: DirectoryPath
    , inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , includeDrafts :: Bool
    , includeTags :: Bool }

build :: Options -> IO ()
build Options{..} = do
    inputPaths <- listDirectoryRecursively inputDirectory
    let inputPathsCategorized =
          categorizePathsByExtension inputPaths
    let postSourcePaths =
          filter ((postsSubDirectory ==) . takeDirectory) $
          fromMaybe [] $
          Map.lookup ".md" inputPathsCategorized
    postsUnordered <- traverse readPost postSourcePaths
    let postsOrdered =
          sortOn (Down . published) postsUnordered
    let postsRendered = map (renderPost includeTags) postsOrdered
    traverse_ (writePage outputDirectory) postsRendered

type DirectoryPath = FilePath

listDirectoryRecursively :: DirectoryPath -> IO [FilePath]
listDirectoryRecursively directory = do
  directoryContents <- listDirectory directory
  fmap concat $ for directoryContents \item -> do
    let itemPath = directory </> item
    isNormalFile <- doesFileExist itemPath
    if isNormalFile then
      return [itemPath]
    else do
      isDirectory <- doesDirectoryExist itemPath
      if isDirectory then
        listDirectoryRecursively itemPath
      else
        return []

type Extension = String

categorizePathsByExtension :: [FilePath] -> Map Extension [FilePath]
categorizePathsByExtension =
    foldr (\path -> Map.insertWith (<>) (takeExtension path) [path]) Map.empty

{- ALTERNATIVE IMPLEMENTATION:
categorizePathsByExtension =
    Map.fromList . map (\path -> (takeExtension path, path))
-}

data Page = Page
    { pageHtml :: Html ()
    , pageRoute :: Route 'Html }

renderPost :: Templates.IncludeTags -> Post -> Page
renderPost includeTags post =
  let postContent = Templates.post includeTags post
   in Page{ pageHtml = Templates.page (title post) postContent
          , pageRoute = Routes.PageR (slug post) }

writePage :: DirectoryPath -> Page -> IO ()
writePage baseDirectory Page{pageHtml, pageRoute} =
    let filepath = baseDirectory </> targetFile pageRoute
     in Html.renderToFile filepath pageHtml

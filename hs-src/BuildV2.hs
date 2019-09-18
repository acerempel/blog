module BuildV2 ( build, Options(..) ) where

import Introit

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Directory
    ( listDirectory, doesDirectoryExist, doesFileExist, createDirectoryIfMissing )
import System.FilePath
    ( (</>), takeDirectory, takeExtension, takeFileName, splitDirectories )

import Lucid ( Html )
import qualified Lucid as Html

import List ( List )
import qualified List
import Post ( Post(..), readPost )
import Routes ( Route(..), targetFile, ContentType(..) )
import qualified Templates

data Options = Options
    { postsSubDirectory :: DirectoryPath
    , inputDirectory :: DirectoryPath
    , outputDirectory :: DirectoryPath
    , excludeDirs :: Set DirectoryPath
    , includeDrafts :: Bool
    , includeTags :: Bool }

build :: Options -> IO ()
build Options{..} = do
    inputPaths <- listDirectoryRecursively inputDirectory excludeDirs
    let inputPathsCategorized =
          categorizePathsByExtension inputPaths
    let postSourcePaths =
          List.filter ((postsSubDirectory ==) . last . splitDirectories . takeDirectory) $
          fromMaybe List.empty $
          Map.lookup ".md" inputPathsCategorized
    postsUnordered <- traverse readPost postSourcePaths
    let postsOrdered =
          List.sortOn (Down . published) postsUnordered
    let postsRendered = fmap (renderPost includeTags) postsOrdered
    traverse_ (writePage outputDirectory) postsRendered

type DirectoryPath = FilePath

listDirectoryRecursively :: DirectoryPath -> Set DirectoryPath -> IO (List FilePath)
listDirectoryRecursively directory excludeDirs = do
  directoryContents <- List.fromList <$> listDirectory directory
  fmap List.concat $ for directoryContents \item -> do
    let itemPath = directory </> item
    isNormalFile <- doesFileExist itemPath
    if isNormalFile then
      return (List.singleton itemPath)
    else do
      isDirectory <- doesDirectoryExist itemPath
      if isDirectory && takeFileName itemPath `Set.notMember` excludeDirs then
        listDirectoryRecursively itemPath excludeDirs
      else
        return List.empty

type Extension = String

categorizePathsByExtension :: List FilePath -> Map Extension (List FilePath)
categorizePathsByExtension =
    foldr (\path -> Map.insertWith (<>) (takeExtension path) (List.singleton path)) Map.empty

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
        containingDirectory = takeDirectory filepath
     in do
         putStrLn $ "Writing to " <> filepath
         createDirectoryIfMissing True containingDirectory
         Html.renderToFile filepath pageHtml

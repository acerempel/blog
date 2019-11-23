module BuildV2 ( build, Options(..) ) where

import Introit

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import System.Directory
    ( listDirectory, doesDirectoryExist, doesFileExist
    , createDirectoryIfMissing, copyFileWithMetadata )
import System.FilePath
    ( (</>), (-<.>), takeDirectory, takeExtension, takeFileName
    , splitDirectories, joinPath )
import System.Process.Typed ( proc, withProcessWait, waitExitCode )

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
          Map.findWithDefault List.empty ".md" inputPathsCategorized
        scssPaths =
          Map.findWithDefault List.empty ".scss" inputPathsCategorized
        assetPaths =
          [".css", ".js", ".jpg", ".png", ".woff", ".woff2"] >>= \extension ->
            Map.findWithDefault List.empty extension inputPathsCategorized
    postsUnordered <- traverse readPost postSourcePaths
    let postsOrdered =
          List.sortOn published postsUnordered
    let postsRendered = fmap (renderPost includeTags) postsOrdered
    traverse_ (writePage outputDirectory) postsRendered
    for_ assetPaths \assetPath -> do
      let targetPath = joinPath $ outputDirectory : tail (splitDirectories assetPath)
      createDirectoryIfMissing True (takeDirectory targetPath)
      copyFileWithMetadata assetPath targetPath
    let scssTargetPath scssSourcePath =
          (joinPath $ outputDirectory : tail (splitDirectories scssSourcePath)) -<.> ".css"
        sassArguments =
          (\scssPath -> scssPath <> ":" <> scssTargetPath scssPath) <$> scssPaths
        sassProcessConfig = proc "sass" ("--no-stop-on-error" : "--no-source-map" : sassArguments)
    _ <- withProcessWait sassProcessConfig waitExitCode
    let homePageContent =
          Templates.page (Templates.archive includeTags postsOrdered)
        homePage = Page{pageHtml = homePageContent, pageRoute = HomeR}
    writePage outputDirectory homePage


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
   in Page{ pageHtml = Templates.page postContent
          , pageRoute = slug post }

writePage :: DirectoryPath -> Page -> IO ()
writePage baseDirectory Page{pageHtml, pageRoute} =
    let filepath = baseDirectory </> targetFile pageRoute
        containingDirectory = takeDirectory filepath
     in do
         putStrLn $ "Writing to " <> filepath
         createDirectoryIfMissing True containingDirectory
         Html.renderToFile filepath pageHtml

module BuildV2 where

import Data.Traversable ( for )
import System.Directory
    ( listDirectory, doesDirectoryExist, doesFileExist )
import System.FilePath
    ( (</>), takeExtension, dropExtension, replaceExtension )

import Lucid ( Html )
import qualified Lucid as Html

import Post ( Post(..), readPost )
import Routes ( Route(..), targetFile )
import Templates ( postLink )
import qualified Templates

type DirectoryPath = FilePath

listDirectoryRecursively :: DirectoryPath -> IO [FilePath]
listDirectoryRecursively baseDirectory = do
    go baseDirectory
    where
      go directory = do
        directoryContents <- listDirectory directory
        fmap concat $ for directoryContents $ \item -> do
          let itemPath = directory </> item
          isNormalFile <- doesFileExist itemPath
          if isNormalFile then
            return [itemPath]
          else do
            isDirectory <- doesDirectoryExist itemPath
            if isDirectory then
              go itemPath
            else
              return []

analyzeSource :: FilePath -> BuildItem
analyzeSource sourcePath =
  let
    buildSource = sourcePath
    buildTarget =
      case takeExtension sourcePath of
      ".md" ->
        PageR (dropExtension sourcePath)
      ".scss" ->
        StylesheetR (replaceExtension sourcePath ".css")
      extension | extension `elem` imageExtensions ->
        ImageR sourcePath
      unknownExtension ->
        error "Que faire??"
   in
    BuildItem{..}

  where imageExtensions = [".jpg", ".png"]

data BuildItem = BuildItem
    { buildSource :: FilePath
    , buildTarget :: Route }

data Page = Page
    { pageHtml :: Html ()
    , pageRoute :: Route }

renderPosts :: [Post] -> [Page]
renderPosts posts =
    case posts of
        [] -> []
        [post1] ->
            renderPost post1 Nothing Nothing : []
        post1 : postsN ->
            go Nothing post1 postsN

    where
        go mbPreceding post1 more =
            case more of
              post2 : evenMore ->
                renderPost post1 mbPreceding (Just (postLink post2)) :
                  go (Just (postLink post1)) post2 evenMore
              [] ->
                renderPost post1 mbPreceding Nothing : []

        renderPost post mbPreceding mbFollowing =
          let postContent = Templates.post post mbPreceding mbFollowing False
           in Page{ pageHtml = Templates.page (title post) postContent
                  , pageRoute = Routes.PageR (slug post) }

writePage :: DirectoryPath -> Page -> IO ()
writePage baseDirectory Page{pageHtml, pageRoute} =
    let filepath = baseDirectory </> targetFile pageRoute
     in Html.renderToFile filepath pageHtml

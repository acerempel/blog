module BuildV2 where

import System.Directory ( listDirectory )
import System.FilePath ( (</>) )

import Lucid ( Html )
import qualified Lucid as Html

import Post ( Post(..), readPost )
import Routes ( Route, targetFile )
import qualified Routes
import Templates ( postLink )
import qualified Templates

type DirectoryPath = FilePath

getAllPosts :: DirectoryPath -> IO [Post]
getAllPosts postSourceDirectory = do
    postSourcePaths <- listDirectory postSourceDirectory
    traverse readPost postSourcePaths

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
                  , pageRoute = Routes.Post (slug post) }

writePage :: DirectoryPath -> Page -> IO ()
writePage baseDirectory Page{pageHtml, pageRoute} =
    let filepath = baseDirectory </> targetFile pageRoute
     in Html.renderToFile filepath pageHtml

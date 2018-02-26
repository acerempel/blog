{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where

import Data.Monoid ( (<>) )
import Data.Text ( isSuffixOf )

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import WaiAppStatic.Types

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let (port, htmlDir) =
            case args of
                [ port, htmlDir ] -> ( read port, htmlDir )
                [ htmlDir ] -> ( 8000, htmlDir )
                [] -> ( 8000, "serve" )
                _ -> error $ "Dodgy args: " <> show args
    run port $ app htmlDir

app :: FilePath -> Application
app htmlDir =
    let defaults = defaultWebAppSettings htmlDir
     in staticApp $ defaults { ssLookupFile = ssLookupFile defaults . route }
  where
    route (map fromPiece -> pieces) = map unsafeToPiece $
        case pieces of
            []
               -> [ "index.html" ]
            [ "archive" ]
               -> [ "archive.html" ]
            [ "posts" , post ]
               | not (".html" `isSuffixOf` post)
               -> [ "posts", post <> ".html" ]
            _  -> pieces


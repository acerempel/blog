{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Main where

import Data.Monoid ( (<>) )
import qualified Data.Text as Text

import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import WaiAppStatic.Types

import System.Environment
import System.FilePath ( hasExtension )

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
      if not (null pieces) && hasExtension (Text.unpack (last pieces)) then
        pieces
      else
        pieces ++ [ "index.html" ]


{-# LANGUAGE OverloadedStrings #-}
module Main where


import Cli
import Data.Aeson
import Data.Maybe (maybe)
import Finder
import Lib
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import qualified Control.Monad as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L


main :: IO ()
main = do
  Opts path types <- parseOpts
  cwd <- getCurrentDirectory
  elmPackage <- BL.readFile (cwd </> "elm-package.json")
  let Just (ElmPackage sourceDirectories) = decode elmPackage
  let paths = L.map ((</>) cwd) $ maybe sourceDirectories pure path
  files <- traverse findFiles paths
  parsed <- traverse parse $ L.concat files
  let filteredFiles = L.filter (containsOneOf types) parsed
  putStrLn $ getJson filteredFiles
  return ()

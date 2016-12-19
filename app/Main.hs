module Main where


import Data.Maybe (mapMaybe)
import Lib
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.FilePath.Find
import System.Environment (getArgs)
import qualified Control.Monad as M
import qualified Data.List as L hiding (find)


main :: IO ()
main = do
  x:args <- getArgs
  types <- return $ mapMaybe toType args
  cwd <- getCurrentDirectory
  files <- find (excludedDirs filePath) (filePath ~~? (cwd </> x) &&? extension ==? ".elm") cwd
  parsed <- M.sequence $ L.map parse files
  filteredFiles <- return $ L.filter (containsOneOf types) parsed
  putStrLn $ getJson filteredFiles
  return ()


excludedDirs :: FindClause FilePath -> FindClause Bool
excludedDirs path =
  path /~? "**/elm-stuff/**"
  &&? path /~? "**/node_modules/**"
  &&? path /~? "**/.git/**"
  &&? path /~? "**/Native/**"

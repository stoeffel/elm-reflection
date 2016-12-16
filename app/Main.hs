module Main where


import Data.Maybe (mapMaybe)
import Lib
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import qualified Control.Monad as M
import qualified Data.List as L


main :: IO ()
main = do
  args <- getArgs
  types <- return $ mapMaybe toType args
  path <- getCurrentDirectory
  files <- getElmFiles path
  filteredFiles <- return $ L.filter (containsOneOf types) files
  putStrLn $ getJson filteredFiles
  return ()

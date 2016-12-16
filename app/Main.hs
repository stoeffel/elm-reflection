module Main where


import Lib
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)
import qualified Data.List as L
import qualified Control.Monad as M


main :: IO ()
main = do
  args <- getArgs
  types <- return $ M.sequence $ L.map toType args
  path <- getCurrentDirectory
  files <- getElmFiles path
  filteredFiles <- return $ L.filter (containsOneOf types) files
  putStrLn $ getJson filteredFiles
  return ()

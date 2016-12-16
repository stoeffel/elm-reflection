module Main where


import Lib
import System.Directory (getCurrentDirectory)


main :: IO ()
main = do
  path <- getCurrentDirectory
  files <- getElmFiles path
  putStrLn $ getJson files
  return ()

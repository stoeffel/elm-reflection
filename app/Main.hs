{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Maybe (mapMaybe)
import Data.Monoid
import Lib
import Options.Applicative
import Options.Applicative.Builder
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.FilePath.Find
import System.Environment (getArgs)
import qualified Control.Monad as M
import qualified Data.List as L hiding (find)
import qualified Data.Text as T


main :: IO ()
main = do
  Opts path types <- execParser opts
  cwd <- getCurrentDirectory
  files <- find (excludedDirs filePath) (filePath ~~? cwd </> path) cwd
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

data Opts = Opts { path :: FilePath, filter :: [Type] }

parser :: Parser Opts
parser =
  Opts <$> strOption (long "path"
                      <> value "**/*.elm"
                     )
       <*> option (maybeReader typeReadM)
                  (long "filter"
                   <> value []
                   <> help ("Possible values: " ++ show [minBound..maxBound :: Type])
                  )
  where
    typeReadM :: String -> Maybe [Type]
    typeReadM = traverse toType . T.splitOn "," . T.pack

opts :: ParserInfo Opts
opts = info (helper <*> parser) fullDesc

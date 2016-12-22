{-# LANGUAGE OverloadedStrings #-}
module Main where


import Data.Aeson
import Data.Maybe (mapMaybe, maybe)
import Data.Monoid
import Lib
import Options.Applicative
import Options.Applicative.Builder
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Find
import qualified Control.Monad as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L hiding (find)
import qualified Data.Text as T


main :: IO ()
main = do
  Opts path types <- execParser opts
  cwd <- getCurrentDirectory
  elmPackage <- BL.readFile (cwd </> "elm-package.json")
  Just (ElmPackage sourceDirectories) <- return $ decode elmPackage
  let paths = L.map ((</>) cwd) $ maybe sourceDirectories pure path
  files <- traverse findFiles paths
  parsed <- traverse parse $ L.concat files
  let filteredFiles = L.filter (containsOneOf types) parsed
  putStrLn $ getJson filteredFiles
  return ()


findFiles :: FilePath -> IO [FilePath]
findFiles =
  find (excludedDirs filePath) (filePath ~~? "**" </> "*.elm")


excludedDirs :: FindClause FilePath -> FindClause Bool
excludedDirs path =
  path /~? "**/elm-stuff/**"
  &&? path /~? "**/node_modules/**"
  &&? path /~? "**/.git/**"
  &&? path /~? "**/Native/**"


data Opts = Opts
  { path :: Maybe FilePath
  , filter :: [Type]
  }

parser :: Parser Opts
parser =
  Opts <$> option (maybeReader pathReadM)(long "path"
                      <> value Nothing
                      <> help "Default is whatever is in your \"source-directories\""
                     )
       <*> option (maybeReader typeReadM)
                  (long "filter"
                   <> value []
                   <> help ("Possible values: " ++ show [minBound..maxBound :: Type])
                  )
  where
    typeReadM :: String -> Maybe [Type]
    typeReadM = traverse toType . T.splitOn "," . T.pack
    pathReadM :: String -> Maybe (Maybe FilePath)
    pathReadM "" = Just Nothing
    pathReadM str = Just $ Just str

opts :: ParserInfo Opts
opts = info (helper <*> parser) fullDesc

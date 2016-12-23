module Finder where


import System.FilePath ((</>))
import System.FilePath.Find


findFiles :: FilePath -> IO [FilePath]
findFiles = find (excludedDirs filePath) (filePath ~~? "**" </> "*.elm")


excludedDirs :: FindClause FilePath -> FindClause Bool
excludedDirs path = path /~? "**" </> "elm-stuff" </> "**"
                &&? path /~? "**" </> "node_modules" </> "**"
                &&? path /~? "**" </> ".git" </> "**"
                &&? path /~? "**" </> "Native" </> "**"

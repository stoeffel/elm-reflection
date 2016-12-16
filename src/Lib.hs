{-# LANGUAGE DeriveGeneric #-}
module Lib (getElmFiles, getJson, toType, containsOneOf) where


import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Maybe (catMaybes)
import Data.Text.Encoding
import GHC.Generics
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.Directory as SD
import qualified System.FilePath.Posix as FP


data Type
  = Test
  | ExposesTests
  | DocTest
  | Css
  deriving (Generic, Show, Eq)


data ElmFile = ElmFile
  { name :: String
  , path :: FilePath
  , types :: [Type]
  } deriving (Generic, Show)


instance ToJSON ElmFile where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON ElmFile


instance ToJSON Type where
  toEncoding = genericToEncoding defaultOptions


instance FromJSON Type


excludeDirs :: [String]
excludeDirs =
  [ "."
  , ".."
  , "elm-stuff"
  , "node_modules"
  ]


getElmFiles :: FilePath -> IO [ElmFile]
getElmFiles topdir = do
  names <- SD.getDirectoryContents topdir
  let properNames = filter (`L.notElem` excludeDirs) names
  paths <- forM properNames (parseIfFile topdir)
  return (concat paths)


parseIfFile :: FilePath -> String -> IO [ElmFile]
parseIfFile topdir name = do
    let path = topdir </> name
    isDirectory <- SD.doesDirectoryExist path
    if isDirectory then
      getElmFiles path
    else if L.isSuffixOf "elm" name then
      fmap (parseFile path) $ readFile path
    else
      return []


-- |
-- >>> parseFile ("src" </> "Foo.elm") $ unlines ["module Foo exposing (..)", "import Test", "import Css"]
-- [ElmFile {name = "Foo", path = "src/Foo.elm", types = [Test,Css]}]
parseFile :: FilePath -> String -> [ElmFile]
parseFile path content = [ ElmFile name path types ] where
  name = (elmModuleName content)
  types = catMaybes
    [ containsElmTest content
    , containsExposesTests content
    , containsDocTest content
    , containsCss content
    ]


-- |
-- >>> elmModuleName "module MyApp.Foo exposing (..)"
-- "MyApp.Foo"
--
-- >>> elmModuleName "port module MyApp.Foo exposing (..)"
-- "MyApp.Foo"
--
-- >>> elmModuleName "effect module MyApp.Foo exposing (..)"
-- "MyApp.Foo"
--
-- >>> elmModuleName "effect module MyApp.Foo \n    exposing (..)"
-- "MyApp.Foo"
elmModuleName :: String -> String
elmModuleName =
  T.unpack
  . T.strip
  . T.replace (T.pack "port ") (T.pack "")
  . T.replace (T.pack "effect ") (T.pack "")
  . T.replace (T.pack "module ") (T.pack "")
  . T.replace (T.pack "\n") (T.pack "")
  . L.head
  . T.splitOn (T.pack "exposing")
  . T.pack


containsSnippet :: Type -> String -> String -> Maybe Type
containsSnippet t token text =
  if L.any (L.isPrefixOf token) $ L.lines text then
    Just t
  else
    Nothing

-- |
-- >>> containsElmTest $ unlines ["module Foo exposing (..)", "import Test"]
-- Just Test
-- >>> containsElmTest $ unlines ["module Foo exposing (..)", "import String"]
-- Nothing
containsElmTest :: String -> Maybe Type
containsElmTest = containsSnippet Test "import Test"


-- |
-- >>> containsExposesTests $ unlines ["module Foo exposing (..)", "tests : Test", "tests = []"]
-- Just ExposesTests
-- >>> containsExposesTests $ unlines ["module Foo exposing (..)", "spec : Test", "spec = []"]
-- Nothing
containsExposesTests :: String -> Maybe Type
containsExposesTests = containsSnippet ExposesTests "tests "


-- |
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "{-|", "    >>> foo 1"]
-- Just DocTest
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "spec : Test", "spec = []"]
-- Nothing
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "{-|", "      >>> foo 1"]
-- Nothing
containsDocTest :: String -> Maybe Type
containsDocTest = containsSnippet DocTest "    >>> "


-- |
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import Css"]
-- Just Css
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import Css as C"]
-- Just Css
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import String"]
-- Nothing
containsCss :: String -> Maybe Type
containsCss = containsSnippet Css "import Css"


-- |
-- >>> toType "foo"
-- Nothing
-- >>> toType "Css"
-- Just Css
toType :: String -> Maybe Type
toType "Css"          = Just Css
toType "Test"         = Just Test
toType "DocTest"      = Just DocTest
toType "ExposesTests" = Just ExposesTests
toType _              = Nothing


-- |
-- >>> containsOneOf ([Css, Test]) $ ElmFile "name" "root" []
-- False
-- >>> containsOneOf [] $ ElmFile "name" "root" []
-- True
-- >>> containsOneOf ([Css, Test]) $ ElmFile "name" "root" [Test]
-- True
containsOneOf :: [Type] -> ElmFile -> Bool
containsOneOf [] _                      = True
containsOneOf oneOf (ElmFile _ _ types) = not $ L.null $ L.intersect oneOf types


getJson :: ToJSON a => a -> String
getJson d = T.unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

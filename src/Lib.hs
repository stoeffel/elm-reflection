{-# LANGUAGE DeriveGeneric #-}
module Lib (getElmFiles, getJson, toType, containsOneOf) where


import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.Encode.Pretty
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
      parseFile path
    else
      return []


parseFile :: FilePath -> IO [ElmFile]
parseFile path = do
  content <- readFile path
  return [
    ElmFile
      (elmModuleName content)
      path $
      L.concat
        [ containsElmTest content
        , containsExposesTests content
        , containsDocTest content
        , containsCss content
        ]
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


containsSnippet :: Type -> String -> String -> [Type]
containsSnippet t token text =
  if L.any (L.isPrefixOf token) $ L.lines text then
    [t]
  else
    []

-- |
-- >>> containsElmTest $ unlines ["module Foo exposing (..)", "import Test"]
-- [Test]
-- >>> containsElmTest $ unlines ["module Foo exposing (..)", "import String"]
-- []
containsElmTest :: String -> [Type]
containsElmTest = containsSnippet Test "import Test"


-- |
-- >>> containsExposesTests $ unlines ["module Foo exposing (..)", "tests : Test", "tests = []"]
-- [ExposesTests]
-- >>> containsExposesTests $ unlines ["module Foo exposing (..)", "spec : Test", "spec = []"]
-- []
containsExposesTests :: String -> [Type]
containsExposesTests = containsSnippet ExposesTests "tests "


-- |
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "{-|", "    >>> foo 1"]
-- [DocTest]
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "spec : Test", "spec = []"]
-- []
-- >>> containsDocTest $ unlines ["module Foo exposing (..)", "{-|", "      >>> foo 1"]
-- []
containsDocTest :: String -> [Type]
containsDocTest = containsSnippet DocTest "    >>> "


-- |
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import Css"]
-- [Css]
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import Css as C"]
-- [Css]
-- >>> containsCss $ unlines ["module Foo exposing (..)", "import String"]
-- []
containsCss :: String -> [Type]
containsCss = containsSnippet Css "import Css"


-- |
-- >>> toType "foo"
-- Nothing
-- >>> toType "Css"
-- Just Css
toType :: String -> Maybe Type
toType "Css" = Just Css
toType "Test" = Just Test
toType "DocTest" = Just DocTest
toType "ExposesTests" = Just ExposesTests
toType _ = Nothing


-- |
-- >>> containsOneOf Nothing $ ElmFile "name" "root" []
-- False
-- >>> containsOneOf (Just [Css, Test]) $ ElmFile "name" "root" []
-- False
-- >>> containsOneOf (Just []) $ ElmFile "name" "root" []
-- True
-- >>> containsOneOf (Just [Css, Test]) $ ElmFile "name" "root" [Test]
-- True
containsOneOf :: Maybe [Type] -> ElmFile -> Bool
containsOneOf maybeOneOf (ElmFile _ _ types) =
  case maybeOneOf of
    Just [] -> True
    Just oneOf -> not $ L.null $ L.intersect oneOf types
    Nothing -> False


getJson :: ToJSON a => a -> String
getJson d = T.unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

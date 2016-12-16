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


containsElmTest :: String -> [Type]
containsElmTest text =
  if L.isInfixOf "import Test " text then
    [Test]
  else
    []


containsExposesTests :: String -> [Type]
containsExposesTests text =
  if L.isInfixOf "tests : Test" text then
    [ExposesTests]
  else
    []

containsDocTest :: String -> [Type]
containsDocTest text =
  if L.isInfixOf "    >>> " text then
    [DocTest]
  else
    []


containsCss :: String -> [Type]
containsCss text =
  if L.isInfixOf "import Css " text then
    [Css]
  else
    []


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
-- >>> containsOneOf (Just [Css, Test]) $ ElmFile "name" "root" [Test]
-- True
containsOneOf :: Maybe [Type] -> ElmFile -> Bool
containsOneOf maybeOneOf (ElmFile _ _ types) =
  case maybeOneOf of
    Just oneOf -> not $ L.null $ L.intersect oneOf types
    Nothing -> False


getJson :: ToJSON a => a -> String
getJson d = T.unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

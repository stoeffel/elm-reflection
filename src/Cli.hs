{-# LANGUAGE OverloadedStrings #-}
module Cli where


import Data.Monoid
import Lib (Type, toType)
import Options.Applicative
import Options.Applicative.Builder
import qualified Data.Text as T


data Opts = Opts
  { path :: Maybe FilePath
  , filter :: [Type]
  }


parseOpts :: IO Opts
parseOpts = execParser opts


parser :: Parser Opts
parser =
  Opts
    <$> option (maybeReader pathReadM)
      (  long "path"
      <> value Nothing
      <> help "Default is whatever is in your \"source-directories\""
      )
    <*> option (maybeReader typeReadM)
      (  long "filter"
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

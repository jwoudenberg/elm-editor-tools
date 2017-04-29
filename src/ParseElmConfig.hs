{-# LANGUAGE OverloadedStrings #-}

module ParseElmConfig
  ( ElmJSON(..)
  , DepsJSON
  , Version
  , readElmJSON
  , readDepsJSON
  ) where

import Control.Applicative (empty)
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map.Strict as Map
import Error
import Path

data ElmJSON = ElmJSON
  { sourceDirectories :: [FilePath]
  } deriving (Show)

type Version = String

type DepsJSON = Map.Map String Version

instance FromJSON ElmJSON where
  parseJSON (Object v) = ElmJSON <$> v .: "source-directories"
  parseJSON _ = empty

readElmJSON :: Path Abs File -> IO (Either Error ElmJSON)
readElmJSON elmJSONPath = do
  jsonString <- ByteString.readFile (toFilePath elmJSONPath)
  let decoded = eitherDecode jsonString
  return $ mapLeft (const decodeError) decoded
  where
    decodeError = CouldNotParseElmJSON (toFilePath elmJSONPath)

readDepsJSON :: Path Abs File -> IO (Either Error DepsJSON)
readDepsJSON depsJSONPath = do
  jsonString <- ByteString.readFile (toFilePath depsJSONPath)
  let decoded = eitherDecode jsonString
  return $ mapLeft (const decodeError) decoded
  where
    decodeError = CouldNotParseDepsJSON (toFilePath depsJSONPath)

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft fn either' =
  case either' of
    Left x -> Left (fn x)
    Right x -> Right x

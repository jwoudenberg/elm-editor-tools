{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- Find root elm-package.json and parse it.
-- Try finding module in one of source directories.
-- Make a list of all dependencies (includes implicits).
-- Find directories matching those dependencies.
-- Parse elm-package.json of dependencies.
-- Try finding module in one of the dependencies' source directories.
-- where
-- The parsing and searching of dependencies elm-package.json is lazy.
-- Native modules are supported.
module Imports
  ( modulePath
  , Error(..)
  ) where

import Control.Applicative (empty, (<|>), liftA2)
import Control.Monad (join)
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.List
import Data.List.Split (splitOn)
import Path
import Path.IO

data ElmJSON = ElmJSON
  { sourceDirectories :: [FilePath]
  } deriving (Show)

instance FromJSON ElmJSON where
  parseJSON (Object v) = ElmJSON <$> v .: "source-directories"
  parseJSON _ = empty

data Error
  = CouldNotFindElmJSON
  | CouldNotParseElmJSON FilePath
  | CouldNotFindModule

modulePath :: FilePath -> String -> IO (Either Error FilePath)
modulePath fromFile moduleName = do
  elmJSONPath <- getDirPath fromFile >>= getElmJSONPath
  elmJSON <- fmap join $ traverse getElmJSON elmJSONPath
  sources <- sequence $ (liftA2 getSourceDirectories) elmJSONPath elmJSON
  finalPath <- fmap join $ traverse (findModuleInRoots moduleName) sources
  return $ fmap toFilePath finalPath

findModuleInRoots :: String
                  -> [Path Abs Dir]
                  -> IO (Either Error (Path Abs File))
findModuleInRoots moduleName rootDirs = do
  let relModulePath = moduleAsPath moduleName
  let candidatePaths = map (relOnRoot relModulePath) rootDirs
  maybePath <- foldr (liftA2 (<|>)) (pure Nothing) candidatePaths
  return $ maybe (Left CouldNotFindModule) pure maybePath

getSourceDirectories :: Path Abs File -> ElmJSON -> IO [Path Abs Dir]
getSourceDirectories elmJSONPath = traverse parseToRoot . sourceDirectories
  where
    parseToRoot :: FilePath -> IO (Path Abs Dir)
    parseToRoot path = do
      relDir <- parseRelDir path
      return $ (parent elmJSONPath) </> relDir

getElmJSON :: Path Abs File -> IO (Either Error ElmJSON)
getElmJSON elmJSONPath = do
  jsonString <- ByteString.readFile (toFilePath elmJSONPath)
  return $ decodeElmJSON elmJSONPath jsonString

decodeElmJSON :: Path Abs File -> ByteString.ByteString -> Either Error ElmJSON
decodeElmJSON elmJSONPath jsonString =
  mapLeft
    (const $ CouldNotParseElmJSON (toFilePath elmJSONPath))
    (eitherDecode jsonString)

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft fn either' =
  case either' of
    Left x -> Left (fn x)
    Right x -> Right x

relOnRoot :: Path Rel File -> Path Abs Dir -> IO (Maybe (Path Abs File))
relOnRoot filePath rootDir = do
  let absFilePath = rootDir </> filePath
  exists <- doesFileExist absFilePath
  if exists
    then return (Just absFilePath)
    else return Nothing

getDirPath :: FilePath -> IO (Path Abs Dir)
getDirPath path = do
  cwd <- getCurrentDir
  absPathToFile <- resolveFile cwd path
  return (parent absPathToFile)

getElmJSONPath :: Path Abs Dir -> IO (Either Error (Path Abs File))
getElmJSONPath dir = do
  let elmJSONPath = dir </> $(mkRelFile "elm-package.json")
  exists <- doesFileExist elmJSONPath
  if exists
    then return $ Right elmJSONPath
    else lookInParent
  where
    parentDir = parent dir
    lookInParent =
      if dir == parentDir
        then return $ Left CouldNotFindElmJSON
        else getElmJSONPath parentDir

moduleAsPath :: String -> Path Rel File
moduleAsPath moduleName = pathToModule
  where
    Right pathToModule = parseRelFile $ baseName ++ extension
    segments = splitOn "." moduleName
    baseName = concat $ intersperse "/" $ segments
    extension =
      if any ((==) "Native") segments
        then ".js"
        else ".elm"

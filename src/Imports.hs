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
  ) where

import Control.Applicative (empty, (<|>))
import Control.Exception.Base (throwIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.List
import Data.List.Split (splitOn)
import Path
import Path.IO

data ElmJSON = ElmJSON
  { sourceDirectories :: [FilePath]
  , exposedModules :: [String]
  } deriving (Show)

instance FromJSON ElmJSON where
  parseJSON (Object v) =
    ElmJSON <$> v .: "source-directories" <*> v .: "exposed-modules"
  parseJSON _ = empty

modulePath :: FilePath -> String -> IO FilePath
modulePath fromFile moduleName = do
  elmJSONPath <- getDirPath fromFile >>= getElmJSONPath
  -- TODO: Add some proper error handling here when Json parsing fails.
  Right elmJSON <- eitherDecode <$> ByteString.readFile (toFilePath elmJSONPath)
  sources <- traverse parseRelDir (sourceDirectories elmJSON)
  -- TODO: Clean up the code below to make it easier to read.
  -- TODO: Search through dependencies too.
  finalPath <-
    foldr
      (<|>)
      (throwIO $ userError "File not found")
      (map (relOnRoot relModulePath . absSourcePath elmJSONPath) sources)
  return (toFilePath finalPath)
  where
    relModulePath = moduleAsPath moduleName
    absSourcePath elmJSONPath relSourcePath =
      parent elmJSONPath </> relSourcePath

relOnRoot :: Path Rel File -> Path Abs Dir -> IO (Path Abs File)
relOnRoot filePath rootDir = do
  exists <- doesFileExist absFilePath
  if exists
    then return absFilePath
    else throwIO (userError "File not found")
  where
    absFilePath = rootDir </> filePath

getDirPath :: FilePath -> IO (Path Abs Dir)
getDirPath path = do
  cwd <- getCurrentDir
  absPathToFile <- resolveFile cwd path
  return (parent absPathToFile)

getElmJSONPath :: Path Abs Dir -> IO (Path Abs File)
getElmJSONPath dir = do
  exists <- doesFileExist elmJSONPath
  if exists
    then return elmJSONPath
    else lookInParent
  where
    elmJSONPath = dir </> $(mkRelFile "elm-package.json")
    parentDir = parent dir
    lookInParent =
      if dir == parentDir
        then throwIO (userError "No elm-package.json found.")
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

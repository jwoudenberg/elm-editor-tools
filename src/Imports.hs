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

import Control.Applicative ((<|>), liftA2)
import Control.Monad (join)
import Data.List
import Data.List.Split (splitOn)
import ElmConfig (ElmJSON(sourceDirectories), readElmJSON)
import Error
import Path
import Path.IO

newtype Candidate =
  Candidate (IO (Maybe (Path Abs File)))

instance Monoid Candidate where
  mempty = Candidate $ pure Nothing
  mappend (Candidate x) (Candidate y) = Candidate (liftA2 (<|>) x y)

modulePath :: FilePath -> String -> IO (Either Error FilePath)
modulePath fromFile moduleName = do
  projectRoot <- getDirPath fromFile >>= findProjectRoot
  let elmJSONPath' = fmap elmJSONPathInDir projectRoot
  elmJSON <- fmap join $ traverse readElmJSON elmJSONPath'
  sources <- sequence $ (liftA2 getSourceDirectories) projectRoot elmJSON
  finalPath <- fmap join $ traverse (findModuleInRoots moduleName) sources
  return $ fmap toFilePath finalPath

findModuleInRoots :: String
                  -> [Path Abs Dir]
                  -> IO (Either Error (Path Abs File))
findModuleInRoots moduleName rootDirs = do
  let relModulePath = moduleAsPath moduleName
  let moduleDirs = fmap (\root -> root </> relModulePath) rootDirs
  maybePath <- unwrap $ foldMap candidate moduleDirs
  return $ maybe (Left CouldNotFindModule) pure maybePath

unwrap :: Candidate -> IO (Maybe (Path Abs File))
unwrap (Candidate innerIO) = innerIO

candidate :: Path Abs File -> Candidate
candidate filePath =
  Candidate $ do
    exists <- doesFileExist filePath
    if exists
      then return (Just filePath)
      else return Nothing

getSourceDirectories :: Path Abs Dir -> ElmJSON -> IO [Path Abs Dir]
getSourceDirectories projectRoot = traverse parseToRoot . sourceDirectories
  where
    parseToRoot :: FilePath -> IO (Path Abs Dir)
    parseToRoot path = do
      relDir <- parseRelDir path
      return $ projectRoot </> relDir

getDirPath :: FilePath -> IO (Path Abs Dir)
getDirPath path = do
  cwd <- getCurrentDir
  absPathToFile <- resolveFile cwd path
  return (parent absPathToFile)

findProjectRoot :: Path Abs Dir -> IO (Either Error (Path Abs Dir))
findProjectRoot dir = do
  exists <- doesFileExist (dir </> elmJSONPath)
  if exists
    then return $ Right dir
    else tryParent
  where
    parentDir = parent dir
    tryParent =
      if dir == parentDir
        then return $ Left CouldNotFindElmJSON
        else findProjectRoot parentDir

elmJSONPath :: Path Rel File
elmJSONPath = $(mkRelFile "elm-package.json")

elmJSONPathInDir :: Path Abs Dir -> Path Abs File
elmJSONPathInDir dir = dir </> elmJSONPath

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

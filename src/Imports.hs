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
  elmJSONPath <- getDirPath fromFile >>= getElmJSONPath
  elmJSON <- fmap join $ traverse readElmJSON elmJSONPath
  sources <- sequence $ (liftA2 getSourceDirectories) elmJSONPath elmJSON
  finalPath <- fmap join $ traverse (findModuleInRoots moduleName) sources
  return $ fmap toFilePath finalPath

findModuleInRoots :: String
                  -> [Path Abs Dir]
                  -> IO (Either Error (Path Abs File))
findModuleInRoots moduleName rootDirs = do
  let relModulePath = moduleAsPath moduleName
  let moduleDirs = fmap (\root -> root </> relModulePath) rootDirs
  let Candidate candidatePath = foldMap candidate moduleDirs
  maybePath <- candidatePath
  return $ maybe (Left CouldNotFindModule) pure maybePath

candidate :: Path Abs File -> Candidate
candidate filePath =
  Candidate $ do
    exists <- doesFileExist filePath
    if exists
      then return (Just filePath)
      else return Nothing

getSourceDirectories :: Path Abs File -> ElmJSON -> IO [Path Abs Dir]
getSourceDirectories elmJSONPath = traverse parseToRoot . sourceDirectories
  where
    parseToRoot :: FilePath -> IO (Path Abs Dir)
    parseToRoot path = do
      relDir <- parseRelDir path
      return $ (parent elmJSONPath) </> relDir

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

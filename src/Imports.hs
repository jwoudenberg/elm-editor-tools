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
import Control.Monad.Except
import Data.List
import Data.List.Split (splitOn)
import ElmConfig (ElmJSON(sourceDirectories), readElmJSON)
import Error
import Path
import Path.IO

type App a = ExceptT Error IO a

newtype Candidate =
  Candidate (IO (Maybe (Path Abs File)))

instance Monoid Candidate where
  mempty = Candidate $ pure Nothing
  mappend (Candidate x) (Candidate y) = Candidate (liftA2 (<|>) x y)

modulePath :: FilePath -> String -> IO (Either Error FilePath)
modulePath fromFile moduleName =
  runExceptT $ do
    let relModulePath = moduleAsPath moduleName
    projectRoot <- getDirPath fromFile >>= findProjectRoot
    candidatePath <- candidateInRoot relModulePath projectRoot
    finalPath <- ensureCandidate candidatePath
    return (toFilePath finalPath)

candidateInRoot :: Path Rel File -> Path Abs Dir -> App Candidate
candidateInRoot relModulePath rootDir = do
  let absElmJSONPath = rootDir </> elmJSONPath
  elmJSON <- ExceptT $ readElmJSON absElmJSONPath
  sourceDirs <- getSourceDirs rootDir elmJSON
  pure (candidateInSources relModulePath sourceDirs)

candidateInSources :: Path Rel File -> [Path Abs Dir] -> Candidate
candidateInSources relModulePath sourceDirs = foldMap candidate moduleDirs
  where
    moduleDirs = fmap (\source -> source </> relModulePath) sourceDirs

ensureCandidate :: Candidate -> App (Path Abs File)
ensureCandidate (Candidate candidatePath) =
  ExceptT $ do
    maybePath <- candidatePath
    return $ maybe (Left CouldNotFindModule) pure maybePath

candidate :: Path Abs File -> Candidate
candidate filePath =
  Candidate $ do
    exists <- doesFileExist filePath
    if exists
      then return (Just filePath)
      else return Nothing

getSourceDirs :: Path Abs Dir -> ElmJSON -> App [Path Abs Dir]
getSourceDirs projectRoot = traverse parseToRoot . sourceDirectories
  where
    parseToRoot :: FilePath -> App (Path Abs Dir)
    parseToRoot path = do
      relDir <- parseRelDir path
      return $ projectRoot </> relDir

getDirPath :: FilePath -> App (Path Abs Dir)
getDirPath path = do
  cwd <- getCurrentDir
  absPathToFile <- resolveFile cwd path
  return (parent absPathToFile)

findProjectRoot :: Path Abs Dir -> App (Path Abs Dir)
findProjectRoot dir = do
  exists <- doesFileExist (dir </> elmJSONPath)
  if exists
    then return dir
    else tryParent
  where
    parentDir = parent dir
    tryParent =
      if dir == parentDir
        then throwError CouldNotFindElmJSON
        else findProjectRoot parentDir

elmJSONPath :: Path Rel File
elmJSONPath = $(mkRelFile "elm-package.json")

moduleAsPath :: String -> Path Rel File
moduleAsPath moduleName = pathToModule
    -- We assume a module name will always result in a valid file path.
  where
    Right pathToModule = parseRelFile $ baseName ++ extension
    segments = splitOn "." moduleName
    baseName = concat $ intersperse "/" $ segments
    extension =
      if any ((==) "Native") segments
        then ".js"
        else ".elm"

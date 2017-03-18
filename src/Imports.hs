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

import Control.Applicative (liftA2)
import Control.Monad.Except
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict as Map
import ElmConfig
       (ElmJSON(sourceDirectories), DepsJSON, readElmJSON, readDepsJSON)
import Error
import Path
import Path.IO

type App a = ExceptT Error IO a

newtype Candidate =
  Candidate (IO (Maybe (Path Abs File)))

instance Monoid Candidate where
  mempty = Candidate $ pure Nothing
  mappend (Candidate current) (Candidate next) =
    Candidate $ do
      maybePath <- current
      -- Stop working the moment you find the first good candidate.
      case maybePath of
        Just path -> pure (Just path)
        Nothing -> next

modulePath :: FilePath -> String -> IO (Either Error FilePath)
modulePath fromFile moduleName =
  runExceptT $ do
    let relModulePath = moduleAsPath moduleName
    projectRoot <- getDirPath fromFile >>= findProjectRoot
    let projectCandidate = candidateInRoot relModulePath projectRoot
    let allDepsCandidate = depsCandidate relModulePath projectRoot
    candidatePath <- (liftA2 mappend) projectCandidate allDepsCandidate
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
getSourceDirs projectRoot elmJSON =
  traverse parseToRoot $ sourceDirectories elmJSON
  where
    parseToRoot :: FilePath -> App (Path Abs Dir)
    parseToRoot path = resolveDir projectRoot path

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

depsCandidate :: Path Rel File -> Path Abs Dir -> App Candidate
depsCandidate relModulePath rootDir = do
  deps <- depPaths rootDir
  candidates <- traverse (candidateInRoot relModulePath) deps
  return (mconcat candidates)

depPaths :: Path Abs Dir -> App [Path Abs Dir]
depPaths rootDir = do
  deps <- ExceptT $ readDepsJSON (rootDir </> depsJSONPath)
  return $ asPaths rootDir deps

asPaths :: Path Abs Dir -> DepsJSON -> [Path Abs Dir]
asPaths rootDir depsJSON = fmap (uncurry $ asPath rootDir) (Map.assocs depsJSON)

asPath :: Path Abs Dir -> String -> String -> Path Abs Dir
asPath rootDir packageName version =
  rootDir </> packagesPath </> pathForPackage </> pathForVersion
  where
    Right pathForPackage = parseRelDir packageName
    Right pathForVersion = parseRelDir version

elmJSONPath :: Path Rel File
elmJSONPath = $(mkRelFile "elm-package.json")

depsJSONPath :: Path Rel File
depsJSONPath = $(mkRelFile "elm-stuff/exact-dependencies.json")

packagesPath :: Path Rel Dir
packagesPath = $(mkRelDir "elm-stuff/packages")

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

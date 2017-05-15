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
module ElmTools.ResolveModule
  ( resolveModule
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Except
import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict as Map
import ElmTools.Error
import ElmTools.ParseConfig
       (ElmJSON(sourceDirectories), DepsJSON, readElmJSON, readDepsJSON)
import Path
import Path.IO
import Strategy (Strategy, choice, (<|>))

type App a = ExceptT Error IO a

type Candidate = Strategy (Path Abs File)

resolveModule :: FilePath -> String -> IO (Either Error FilePath)
resolveModule importedFromFile moduleName =
  runExceptT $ do
    let relModulePath = moduleAsPath moduleName
    localRoot <- getDirPath importedFromFile >>= findLocalRoot
    let projectRoot = findProjectRoot localRoot
    let projectCandidate = candidateInRoot relModulePath localRoot
    let allDepsCandidate = depsCandidate relModulePath projectRoot
    candidatePath <- (liftA2 (<|>)) projectCandidate allDepsCandidate
    finalPath <- runCandidate candidatePath
    return (toFilePath finalPath)

candidateInRoot :: Path Rel File -> Path Abs Dir -> App Candidate
candidateInRoot relModulePath rootDir = do
  sourceDirs <- getSourceDirs rootDir
  pure (candidateInSourceDirs relModulePath sourceDirs)

candidateInSourceDirs :: Path Rel File -> [Path Abs Dir] -> Candidate
candidateInSourceDirs relModulePath sourceDirs = choice candidates
  where
    fullPath source = source </> relModulePath
    candidates = fmap (candidate . fullPath) sourceDirs

runCandidate :: Candidate -> App (Path Abs File)
runCandidate candidate' =
  ExceptT $ do
    path <- candidate'
    return $ maybe (Left CouldNotFindModule) pure path

candidate :: Path Abs File -> Candidate
candidate filePath = do
  exists <- doesFileExist filePath
  if exists
    then return (Just filePath)
    else return Nothing

getSourceDirs :: Path Abs Dir -> App [Path Abs Dir]
getSourceDirs localRoot = do
  let absElmJSONPath = localRoot </> elmJSONPath
  elmJSON <- ExceptT $ readElmJSON absElmJSONPath
  traverse withRoot $ sourceDirectories elmJSON
  where
    withRoot :: FilePath -> App (Path Abs Dir)
    withRoot path = resolveDir localRoot path

getDirPath :: FilePath -> App (Path Abs Dir)
getDirPath path = do
  cwd <- getCurrentDir
  absPathToFile <- resolveFile cwd path
  return (parent absPathToFile)

findLocalRoot :: Path Abs Dir -> App (Path Abs Dir)
findLocalRoot dir = do
  exists <- doesFileExist (dir </> elmJSONPath)
  if exists
    then return dir
    else tryParent
  where
    parentDir = parent dir
    tryParent =
      if dir == parentDir
        then throwError CouldNotFindElmJSON
        else findLocalRoot parentDir

findProjectRoot :: Path Abs Dir -> Path Abs Dir
findProjectRoot localRoot
  -- Check whether the localRoot is a dependency, meaning it has this form:
  -- <projectRoot>/elm-stuff/packages/<user>/<package-name>/<version>
 = do
  let maybeElmStuffPath = parent $ parent $ parent $ parent $ localRoot
  if dirname maybeElmStuffPath == $(mkRelDir "elm-stuff")
    then parent maybeElmStuffPath
    else localRoot

depsCandidate :: Path Rel File -> Path Abs Dir -> App Candidate
depsCandidate relModulePath projectRoot = do
  deps <- depPaths projectRoot
  candidates <- traverse (candidateInRoot relModulePath) deps
  return (choice candidates)

depPaths :: Path Abs Dir -> App [Path Abs Dir]
depPaths projectRoot = do
  deps <- ExceptT $ readDepsJSON (projectRoot </> depsJSONPath)
  return $ asPaths projectRoot deps

asPaths :: Path Abs Dir -> DepsJSON -> [Path Abs Dir]
asPaths projectRoot depsJSON =
  fmap (uncurry $ asPath projectRoot) (Map.assocs depsJSON)

asPath :: Path Abs Dir -> String -> String -> Path Abs Dir
asPath projectRoot packageName version =
  projectRoot </> packagesPath </> pathForPackage </> pathForVersion
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

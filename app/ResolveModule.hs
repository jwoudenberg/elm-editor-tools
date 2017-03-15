{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib (modulePath, Error(..))
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  (fromFile, moduleName) <- getArgs
  resolveModule fromFile moduleName

getArgs :: IO (FilePath, String)
getArgs = do
  args <- System.Environment.getArgs
  case args of
    [fromFile, moduleName] -> pure (fromFile, moduleName)
    _ -> do
      exitWithError usage

resolveModule :: FilePath -> String -> IO ()
resolveModule fromFile moduleName = do
  resolvedPath' <- modulePath fromFile moduleName
  case resolvedPath' of
    Left Lib.CouldNotFindElmJSON ->
      exitWithError $
      "No elm-package.Rson file found. Is \"" ++
      fromFile ++ "\" in an instantiated elm project?"
    Left (Lib.CouldNotParseElmJSON elmJSONPath) ->
      exitWithError $
      "Could not parse elm-package.json found at: \"" ++ elmJSONPath
    Right resolvedPath -> putStrLn resolvedPath

usage :: String
usage = "Usage:\n  elm-resolve-module <from-path> <module-name>"

exitWithError :: String -> IO a
exitWithError msg = do
  _ <- putStrLn msg
  System.Exit.exitFailure

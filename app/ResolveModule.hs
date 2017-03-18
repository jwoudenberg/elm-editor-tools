{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib (modulePath, Error(..))
import qualified System.Environment
import System.Exit (die)

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
      die usage

resolveModule :: FilePath -> String -> IO ()
resolveModule fromFile moduleName = do
  resolvedPath' <- modulePath fromFile moduleName
  case resolvedPath' of
    Left Lib.CouldNotFindElmJSON ->
      die $
      "No elm-package.Rson file found. Is \"" ++
      fromFile ++ "\" in an instantiated elm project?"
    Left (Lib.CouldNotParseElmJSON elmJSONPath) ->
      die $ "Could not parse elm-package.json found at: " ++ elmJSONPath
    Left (Lib.CouldNotParseDepsJSON depsJSONPath) ->
      die $ "Could not parse exact-dependencies.json found at: " ++ depsJSONPath
    Left Lib.CouldNotFindModule -> die $ "Could not find module: " ++ moduleName
    Right resolvedPath -> putStrLn resolvedPath

usage :: String
usage = "Usage:\n  elm-resolve-module <from-path> <module-name>"

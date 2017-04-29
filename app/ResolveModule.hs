{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Lib
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
      System.Exit.die usage

resolveModule :: FilePath -> String -> IO ()
resolveModule fromFile moduleName = do
  resolvedPath' <- Lib.resolveModule fromFile moduleName
  case resolvedPath' of
    Left err -> System.Exit.die (show err)
    Right resolvedPath -> putStrLn resolvedPath

usage :: String
usage = "Usage:\n  elm-resolve-module <from-path> <module-name>"

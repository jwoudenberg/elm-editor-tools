{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib (modulePath)
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
    Left err -> die (show err)
    Right resolvedPath -> putStrLn resolvedPath

usage :: String
usage = "Usage:\n  elm-resolve-module <from-path> <module-name>"

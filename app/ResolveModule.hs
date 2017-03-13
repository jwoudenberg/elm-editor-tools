{-# OPTIONS_GHC -Wall #-}

module Main where

import Lib (modulePath)
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case args of
    [fromFile, moduleName] -> resolveModule fromFile moduleName
    _ -> do
      _ <- putStrLn "Usage:\n  elm-resolve-module <from-path> <module-name>"
      System.Exit.exitFailure

resolveModule :: FilePath -> String -> IO ()
resolveModule fromFile moduleName = modulePath fromFile moduleName >>= putStrLn

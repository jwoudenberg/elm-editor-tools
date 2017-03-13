{-# OPTIONS_GHC -Wall #-}

module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import Lib
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  files <- System.Environment.getArgs
  case files of
    [] -> putStrLn ("Please provide the path to an elm file")
    first:_ -> parseFile first
  return ()

parseFile :: String -> IO ()
parseFile fileName_ = do
  result <- elmParser fileName_
  case result of
    Left parseError -> do
      _ <- putStrLn ("Parsing failed with: " ++ (show parseError))
      System.Exit.exitFailure
    Right definitions ->
      Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.encode definitions)

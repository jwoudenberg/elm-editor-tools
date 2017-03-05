module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  files <- getArgs
  traverse parseFile files
  return ()

parseFile :: String -> IO ()
parseFile fileName = do
  result <- elmParser fileName
  putStrLn ("\n# " ++ fileName)
  putStrLn (show $ result)

module Main where

import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import qualified ElmTools
import qualified System.Environment
import qualified System.Exit

main :: IO ()
main = do
  files <- System.Environment.getArgs
  case files of
    [filePath, name] -> find filePath name
    _ -> putStrLn ("Usage: elm-find-definition <file-path> <name> ")
  return ()

find :: FilePath -> String -> IO ()
find filePath name = do
  result <- ElmTools.findDefinition filePath name
  case result of
    Left err -> System.Exit.die $ show err
    Right definitions ->
      Data.ByteString.Lazy.Char8.putStrLn (Data.Aeson.encode definitions)

module Main where

import Lib

input :: String
input =
  "import Foo\n\
  \I can just write stuff here without effect\n\
  \import Bar\n\
  \import Test.Something\n"

main :: IO ()
main = do
  result <- elmParser  "/Users/jasper/projects/elm-editor-tools/parse-elm-experiment/app/test.elm"
  putStrLn (show $ result)

{-# OPTIONS_GHC -Wall #-}

import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "parseTests"
    [ t "base case" "foo : Int" [def "foo" 1 1]
    , t "leading space" " foo : Int" [def "foo" 1 2]
    , t "leading tab" "\tfoo : Int" [def "foo" 1 9]
    , t "nowhitespace" "foo:Int" [def "foo" 1 1]
    , t "much whitespace" "foo   :\tInt" [def "foo" 1 1]
    , t "broken across lines" "foo \n: Int" [def "foo" 1 1]
    , t "non-first line" "a line\nanother line\nfoo : Int" [def "foo" 3 1]
    , t
        "tailing content"
        "foo : Int and some more stuff\nanother line"
        [def "foo" 1 1]
    ]

def :: String -> Int -> Int -> Definition
def name line_ column_ = TopFunction name (Location fileName_ line_ column_)

fileName_ :: String
fileName_ = "myFile"

t :: String -> String -> [Definition] -> TestTree
t description content definitions =
  testCase description $
  assertEqual description (Right definitions) (parseString fileName_ content)

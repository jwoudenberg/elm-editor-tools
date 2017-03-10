import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "parseTests" ["foo : Int\n" ==> [def "foo" 1 1]]

def :: String -> Int -> Int -> Definition
def name line column = Definition name fileName_ line column

fileName_ :: String
fileName_ = "myFile"

(==>) :: String -> [Definition] -> TestTree
(==>) content definitions =
  testCase content $
  assertEqual
    "Should parse correctly"
    (Right definitions)
    (parseString fileName_ content)

import ElmTools.ParseModule
import ElmTools.ParseModule.Types
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "parseTests"
    [ topLevelFunctionTests
    , sumTypeTests
    , typeAliasTests
    , combinationTests
    , commentTests
    , importTests
    ]

topLevelFunctionTests :: TestTree
topLevelFunctionTests =
  testGroup
    "top level functions"
    [ t "top level function" "foo = 42" [fn "foo" 1 1]
    , t "with arguments" "foo (One thing) { field } int = 42" [fn "foo" 1 1]
    , t "no whitespace" "foo=42" [fn "foo" 1 1]
    , t "much whitespace" "foo   =\t42" [fn "foo" 1 1]
    , t "broken across lines" "foo =\n 42" [fn "foo" 1 1]
    , t "non-first line" "a line\nanother line\nfoo = 42" [fn "foo" 3 1]
    , t
        "tailing content"
        "foo = 42 and some more stuff\nanother line"
        [fn "foo" 1 1]
    , t "infix operator" "(<>) = something" [fn "<>" 1 2]
    , t
        "destructured record"
        "{ foo, bar } = something"
        [fn "foo" 1 3, fn "bar" 1 8]
    , t
        "destructured tuple"
        "( foo, bar ) = something"
        [fn "foo" 1 3, fn "bar" 1 8]
    , t
        "nested destructured tuple"
        "( ( foo, bar ), { baz }) = something"
        [fn "foo" 1 5, fn "bar" 1 10, fn "baz" 1 19]
    ]

sumTypeTests :: TestTree
sumTypeTests =
  testGroup
    "sum types"
    [ t "sum type" "type Foo = Bar" [typeConstructor "Bar" 1 12]
    , t
        "with multiple type constructors"
        "type Foo = Bar | Baz"
        [typeConstructor "Bar" 1 12, typeConstructor "Baz" 1 18]
    , t "minimal whitespace" "type Foo=Bar" [typeConstructor "Bar" 1 10]
    , t "much whitespace" "type  Foo  =  Bar" [typeConstructor "Bar" 1 15]
    , t "broken across lines" "type Foo =\n Bar" [typeConstructor "Bar" 2 2]
    , t
        "non-first line"
        "a line\nanother line\ntype Foo = Bar"
        [typeConstructor "Bar" 3 12]
    , t
        "tailing content"
        "type Foo = Bar and some more stuff\nanother line"
        [typeConstructor "Bar" 1 12]
    , t
        "with type parameters"
        "type Foo = Bar a | Baz one (Maybe Int)"
        [typeConstructor "Bar" 1 12, typeConstructor "Baz" 1 20]
    ]

typeAliasTests :: TestTree
typeAliasTests =
  testGroup
    "type aliases"
    [ t "type alias" "type alias Foo = Bar" [typeAlias "Foo" 1 12]
    , t "much whitespace" "type  alias  Foo  =  Bar" [typeAlias "Foo" 1 14]
    , t
        "broken across lines"
        "type\n alias\n Foo\n =\n Bar"
        [typeAlias "Foo" 3 2]
    , t
        "non-first line"
        "a line\nanother line\ntype alias Foo = Bar"
        [typeAlias "Foo" 3 12]
    , t
        "tailing content"
        "type alias Foo = Bar and some more stuff\nanother line"
        [typeAlias "Foo" 1 12]
    , t
        "with type parameters"
        "type alias Foo a b = Bar (Maybe Int) extra"
        [typeAlias "Foo" 1 12]
    ]

combinationTests :: TestTree
combinationTests =
  testGroup
    "multiple definitions"
    [ t
        "sum type followed by top level function"
        "type Foo = Bar\nfoo = 42"
        [typeConstructor "Bar" 1 12, fn "foo" 2 1]
    , t
        "top level function followed by sum type"
        "foo = 42\ntype Foo = Bar"
        [fn "foo" 1 1, typeConstructor "Bar" 2 12]
    , t
        "top level function followed by type alias"
        "foo = 42\ntype alias Foo = Bar"
        [fn "foo" 1 1, typeAlias "Foo" 2 12]
    , t
        "type alias followed by top level function"
        "type alias Foo = Bar\nfoo = 42"
        [typeAlias "Foo" 1 12, fn "foo" 2 1]
    , t
        "type alias followed by sum type"
        "type alias Foo = Bar\ntype Bar = Baz"
        [typeAlias "Foo" 1 12, typeConstructor "Baz" 2 12]
    , t
        "sum type folloed by type alias"
        "type Bar = Baz\ntype alias Foo = Bar"
        [typeConstructor "Baz" 1 12, typeAlias "Foo" 2 12]
    ]

commentTests :: TestTree
commentTests =
  testGroup
    "comments"
    [ t "commented definition" "{-\nfoo = 42-}" []
    , t
        "definition after comment"
        "{-some stuff in here-}\nfoo = 42"
        [fn "foo" 2 1]
    , t "comment until end of file" "{-\nfoo = 42" []
    , t "nested comment blocks" "{-{--}\nfoo = 42-}" []
    ]

importTests :: TestTree
importTests =
  testGroup
    "imports functions"
    [ t "simple" "import Foo.Bar" [imprt "Foo.Bar" "Foo.Bar" (Selected [])]
    , t "aliased" "import Foo.Bar as Bar" [imprt "Foo.Bar" "Bar" (Selected [])]
    , t
        "exposing"
        "import Foo.Bar exposing (One, two)"
        [imprt "Foo.Bar" "Foo.Bar" (Selected ["One", "two"])]
    , t
        "exposing everything"
        "import Foo.Bar exposing (..)"
        [imprt "Foo.Bar" "Foo.Bar" All]
    , t
        "exposing some sum type constructors"
        "import Foo.Bar exposing (One(Two))"
        [imprt "Foo.Bar" "Foo.Bar" (Selected ["Two"])]
    , t
        "exposing all sum type constructors"
        "import Foo.Bar exposing (One(..))"
        [imprt "Foo.Bar" "Foo.Bar" (Selected ["One(..)"])]
    , t
        "much whitespace"
        "import    Foo.Bar  \tas    Bar    exposing   ( One  ,  Two ( Three ) )"
        [imprt "Foo.Bar" "Bar" (Selected ["One", "Three"])]
    , t
        "broken across lines"
        "import Foo.Bar exposing\n (\n One, two)"
        [imprt "Foo.Bar" "Foo.Bar" (Selected ["One", "two"])]
    ]

fn :: String -> Int -> Int -> Declaration
fn name' line_ column_ =
  Definition $ DefinitionC Function (Location fileName_ line_ column_) name'

typeConstructor :: String -> Int -> Int -> Declaration
typeConstructor name' line_ column_ =
  Definition $
  DefinitionC TypeConstructor (Location fileName_ line_ column_) name'

typeAlias :: String -> Int -> Int -> Declaration
typeAlias name' line_ column_ =
  Definition $ DefinitionC TypeAlias (Location fileName_ line_ column_) name'

imprt :: String -> String -> ExposedNames -> Declaration
imprt qualiedName localName_ exposing_ =
  Import $ ImportC qualiedName localName_ exposing_

fileName_ :: String
fileName_ = "myFile"

t :: String -> String -> [Declaration] -> TestTree
t description content definitions =
  testCase description $
  assertEqual description (Right definitions) (parseString fileName_ content)

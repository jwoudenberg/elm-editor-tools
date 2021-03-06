import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
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
    , exportTests
    ]

topLevelFunctionTests :: TestTree
topLevelFunctionTests =
  testGroup
    "top level functions"
    [ t "top level function" "foo = 42" [fn "foo" 1 1]
    , t "with numbers in name" "foo2 = 42" [fn "foo2" 1 1]
    , t "with arguments" "foo (One thing) { field } int = 42" [fn "foo" 1 1]
    , t "no whitespace" "foo=42" [fn "foo" 1 1]
    , t "much whitespace" "foo   =\t42" [fn "foo" 1 1]
    , t "broken across lines" "foo =\n 42" [fn "foo" 1 1]
    , t "non-first line" "a line\nanother line\nfoo = 42" [fn "foo" 3 1]
    , t
        "tailing content"
        "foo = 42 and some more stuff\nanother line"
        [fn "foo" 1 1]
    , t "infix operator" "(<>) = something" [fn "<>" 1 1]
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
    [ t "sum type" "type Foo = Bar" [typeConstructor "Foo" "Bar" 1 12]
    , t
        "with numbers in name"
        "type Foo2 = Bar2"
        [typeConstructor "Foo2" "Bar2" 1 13]
    , t
        "with multiple type constructors"
        "type Foo = Bar | Baz"
        [typeConstructor "Foo" "Bar" 1 12, typeConstructor "Foo" "Baz" 1 18]
    , t "minimal whitespace" "type Foo=Bar" [typeConstructor "Foo" "Bar" 1 10]
    , t "much whitespace" "type  Foo  =  Bar" [typeConstructor "Foo" "Bar" 1 15]
    , t
        "broken across lines"
        "type Foo =\n Bar"
        [typeConstructor "Foo" "Bar" 2 2]
    , t
        "non-first line"
        "a line\nanother line\ntype Foo = Bar"
        [typeConstructor "Foo" "Bar" 3 12]
    , t
        "tailing content"
        "type Foo = Bar and some more stuff\nanother line"
        [typeConstructor "Foo" "Bar" 1 12]
    , t
        "with type parameters"
        "type Foo a (Maybe c) = Bar a | Baz one (Maybe Int)"
        [typeConstructor "Foo" "Bar" 1 24, typeConstructor "Foo" "Baz" 1 32]
    ]

typeAliasTests :: TestTree
typeAliasTests =
  testGroup
    "type aliases"
    [ t "type alias" "type alias Foo = Bar" [typeAlias "Foo" 1 12]
    , t "with numbers in name" "type alias Foo2 = Bar2" [typeAlias "Foo2" 1 12]
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
        [typeConstructor "Foo" "Bar" 1 12, fn "foo" 2 1]
    , t
        "top level function followed by sum type"
        "foo = 42\ntype Foo = Bar"
        [fn "foo" 1 1, typeConstructor "Foo" "Bar" 2 12]
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
        [typeAlias "Foo" 1 12, typeConstructor "Bar" "Baz" 2 12]
    , t
        "sum type folloed by type alias"
        "type Bar = Baz\ntype alias Foo = Bar"
        [typeConstructor "Bar" "Baz" 1 12, typeAlias "Foo" 2 12]
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
    , t "comments with hyphens" "{- -\ntype of-}" []
    ]

importTests :: TestTree
importTests =
  testGroup
    "imports functions"
    [ t "simple" "import Foo.Bar" [imprt "Foo.Bar" "Foo.Bar" (selected [])]
    , t "aliased" "import Foo.Bar as Bar" [imprt "Foo.Bar" "Bar" (selected [])]
    , t
        "exposing"
        "import Foo.Bar exposing (One, two)"
        [imprt "Foo.Bar" "Foo.Bar" (selected [Tipe "One", NonTipe "two"])]
    , t
        "exposing everything"
        "import Foo.Bar exposing (..)"
        [imprt "Foo.Bar" "Foo.Bar" All]
    , t
        "exposing some sum type constructors"
        "import Foo.Bar exposing (One(Two))"
        [ imprt
            "Foo.Bar"
            "Foo.Bar"
            (selected [TipeWithSingleConstructor "One" "Two"])
        ]
    , t
        "exposing all sum type constructors"
        "import Foo.Bar exposing (One(..))"
        [imprt "Foo.Bar" "Foo.Bar" (selected [TipeWithConstructors "One"])]
    , t
        "much whitespace"
        "import    Foo.Bar  \tas    Bar    exposing   ( One  ,  Two ( Three ) )"
        [ imprt
            "Foo.Bar"
            "Bar"
            (selected [Tipe "One", TipeWithSingleConstructor "Two" "Three"])
        ]
    , t
        "broken across lines"
        "import Foo.Bar exposing\n (\n One, two)"
        [imprt "Foo.Bar" "Foo.Bar" (selected [Tipe "One", NonTipe "two"])]
    , t
        "infix operators"
        "import Foo.Bar exposing ((?))"
        [imprt "Foo.Bar" "Foo.Bar" (selected [NonTipe "?"])]
    ]

exportTests :: TestTree
exportTests =
  testGroup
    "exported names"
    [ testExport
        "exporting everything"
        "module Foo exposing (..)\nfoo = 42\ntype Bla = Baz"
        ["foo", "Baz"]
    , testExport "exporting nothing" "module Foo exposing ()\nfoo = 42" []
    , testExport
        "exporting selected names"
        "module Foo exposing (bar)\nfoo = 42\nbar= 42"
        ["bar"]
    , testExport
        "exporting selected type constructors"
        "module Foo exposing (Foo(Bar))\ntype Foo = Bar | Baz"
        ["Bar"]
    , testExport
        "exporting all type constructors"
        "module Foo exposing (Foo(..))\ntype Foo = Bar | Baz"
        ["Bar", "Baz"]
    , testExport
        "infix operators"
        "module Foo exposing ((?))\n(?) a b = a"
        ["?"]
    ]

fn :: String -> Int -> Int -> Declaration
fn name' line_ column_ =
  Definition $ DefinitionC Function Global location' name'
  where
    location' = Location fileName_ line_ column_

typeConstructor :: String -> String -> Int -> Int -> Declaration
typeConstructor typeName name' line_ column_ =
  Definition $ DefinitionC (TypeConstructor typeName) Global location' name'
  where
    location' = Location fileName_ line_ column_

typeAlias :: String -> Int -> Int -> Declaration
typeAlias name' line_ column_ =
  Definition $ DefinitionC TypeAlias Global location' name'
  where
    location' = Location fileName_ line_ column_

imprt :: String -> String -> ExposedNames -> Declaration
imprt qualiedName localName_ exposing_ =
  Import $ ImportC qualiedName localName_ exposing_

fileName_ :: String
fileName_ = "myFile"

t :: String -> String -> [Declaration] -> TestTree
t description content definitions =
  testCase description $
  assertEqual description (Right definitions) (parseString fileName_ content)

testExport :: String -> String -> [String] -> TestTree
testExport description content expectedExportedNames =
  testCase description $ assertEqual description expected actual
  where
    expected = Right $ Set.fromList expectedExportedNames
    actual = fmap exportedNames (parseString fileName_ content)

exportedDef :: Declaration -> Maybe Definition
exportedDef declaration =
  case declaration of
    Definition definition ->
      case scope definition of
        Global -> Nothing
        Exported -> Just definition
    Import _ -> Nothing

exportedNames :: [Declaration] -> Set.Set String
exportedNames = Set.fromList . fmap name . mapMaybe exportedDef

selected :: [ExposedName] -> ExposedNames
selected = Selected . Set.fromList

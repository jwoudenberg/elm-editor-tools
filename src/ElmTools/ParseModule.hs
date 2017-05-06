module ElmTools.ParseModule
  ( elmParser
  , parseString
  ) where

import Control.Monad (join)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import ElmTools.ParseModule.Types
import Text.Parsec
import Text.Parsec.Indent

type DefParser result = IndentParser String ParseState result

data ParseState = ParseState
  { exportedNames :: ExposedNames
  }

elmParser :: FilePath -> IO (Either ParseError [Declaration])
elmParser filePath = do
  input <- readFile filePath
  return (parseString filePath input)

parseString :: FilePath -> String -> Either ParseError [Declaration]
parseString filePath fileContent =
  runIndentParser declarations startState filePath fileContent
    -- We assume nothing is exported until told otherwise.
  where
    startState = ParseState (Selected Set.empty)

declarations :: DefParser [Declaration]
declarations = do
  topLevel
  result <- mconcat <$> manyTill line_ eof
  return result

line_ :: DefParser [Declaration]
line_ = do
  (try $ option [] declaration) <* restOfLine
  where
    declaration =
      choice
        [ moduleDefinition *> pure []
        , pure <$> Import <$> importStatement
        , pure <$> Definition <$> typeAlias
        , fmap Definition <$> sumType
        , fmap Definition <$> destructuredAssignment
        , pure <$> Definition <$> topFunction
        ]

restOfLine :: DefParser ()
restOfLine = dump $ manyTill (try comment <|> dump anyChar) endOfFileOrLine

comment :: DefParser ()
comment = string "{-" *> commentContentsAndEnd

commentContentsAndEnd :: DefParser ()
commentContentsAndEnd =
  dump $ manyTill (try comment <|> dump anyChar) commentBlockEnd

commentBlockEnd :: DefParser ()
commentBlockEnd = eof <|> dump (try $ string "-}")

moduleDefinition :: DefParser ()
moduleDefinition = do
  exportedNames' <-
    (try $ string "module") *> whitespace1 *> moduleName *> whitespace1 *>
    string "exposing" *>
    whitespace1 *>
    exposedList
  _ <- modifyState (\s -> s {exportedNames = exportedNames'})
  return ()

definition :: DefinitionType -> Location -> String -> DefParser Definition
definition definitionType' location' name' = do
  exportedNames' <- exportedNames <$> getState
  let scope' =
        case (exportedNames', definitionType') of
          (All, _) -> Exported
          (Selected exported, TypeConstructor typeName) ->
            if (Set.member (typeName ++ "(..)") exported) ||
               (Set.member (typeName ++ "." ++ name') exported)
              then Exported
              else Global
          (Selected exported, _) ->
            if Set.member name' exported
              then Exported
              else Global
  return $ DefinitionC definitionType' scope' location' name'

destructuredAssignment :: DefParser [Definition]
destructuredAssignment = try $ destructuredContent <* whitespace <* char '='

destructuredContent :: DefParser [Definition]
destructuredContent =
  choice [destructuredRecord, destructuredTuple, pure <$> variable]

destructuredRecord :: DefParser [Definition]
destructuredRecord = inCurlyBraces $ sepBy variable (try comma)

destructuredTuple :: DefParser [Definition]
destructuredTuple = inBraces $ join <$> sepBy destructuredContent (try comma)

topFunction :: DefParser Definition
topFunction =
  try $ infixOperator <|> variable <* whitespace <* arguments <* char '='

variable :: DefParser Definition
variable = join $ (definition Function) <$> getLocation <*> lowerCasedWord

sumType :: DefParser [Definition]
sumType = do
  typeKey *> (typeDefinition <* arguments <* equalSign) >>= constructors
  where
    typeKey = (try $ string "type") *> whitespace1
    equalSign = whitespace *> char '=' *> whitespace
    constructors typeName =
      sepBy
        (typeConstructor typeName)
        (try $ whitespace >> char '|' >> whitespace)

importStatement :: DefParser Import
importStatement =
  importKey *> (pure importDef) <*> moduleName <*> asClause <*> exposingClause
  where
    importKey = (try $ string "import") *> whitespace1
    asClause =
      optionMaybe $
      try $ whitespace1 *> string "as" *> whitespace1 *> capitalizedWord
    exposingClause =
      optionMaybe $
      try $ whitespace1 *> string "exposing" *> whitespace1 *> exposedList
    importDef name' maybeAlias maybeExposed =
      ImportC
        name'
        (fromMaybe name' maybeAlias)
        (fromMaybe (Selected Set.empty) maybeExposed)

typeAlias :: DefParser Definition
typeAlias = do
  _ <- typeAliasKeyword
  join $ (definition TypeAlias) <$> getLocation <*> capitalizedWord
  where
    typeAliasKeyword =
      (try $ string "type" *> whitespace1 *> string "alias") *> whitespace1

typeDefinition :: DefParser String
typeDefinition = do
  baseType <- capitalizedWord
  typeParams <-
    optionMaybe $ try $ whitespace1 >> sepBy lowerCasedWord whitespace1
  return $ concat $ intersperse " " $ baseType : (maybe [] id typeParams)

typeConstructor :: String -> DefParser Definition
typeConstructor typeName =
  join $ typeConstructor' <$> getLocation <*> capitalizedWord <* arguments
  where
    nameWithoutParams = head $ splitOn " " typeName
    typeConstructor' = (definition $ TypeConstructor nameWithoutParams)

getLocation :: DefParser Location
getLocation = do
  sourcePos <- getPosition
  pure $
    Location
    { fileName = sourceName sourcePos
    , line = sourceLine sourcePos
    , column = sourceColumn sourcePos
    }

lowerCasedWord :: DefParser String
lowerCasedWord = do
  initial <- lower
  rest <- many alphaNum
  return (initial : rest)

capitalizedWord :: DefParser String
capitalizedWord = do
  initial <- upper
  rest <- many alphaNum
  return (initial : rest)

moduleName :: DefParser String
moduleName = mconcat <$> intersperse "." <$> sepBy capitalizedWord (string ".")

exposedList :: DefParser ExposedNames
exposedList =
  choice
    [ try $ exposeAll *> pure All
    , Selected . Set.fromList . mconcat <$>
      (inBraces $ sepBy exposedValue (try comma))
    ]
  where
    exposedValue =
      choice
        [exposedSumType, pure <$> lowerCasedWord, pure <$> inBraces operator]

exposedSumType :: DefParser [String]
exposedSumType =
  choice $
  fmap
    try
    [ (fmap . joinWithDot) <$> (capitalizedWord <* whitespace) <*>
      exposedConstructors
    , pure <$> (pure (++) <*> capitalizedWord <* whitespace <*> exposeAll)
    , pure <$> capitalizedWord
    ]
  where
    exposedConstructors = inBraces (sepBy capitalizedWord (try comma))
    joinWithDot a b = a ++ "." ++ b

exposeAll :: DefParser String
exposeAll = inBraces (string "..") *> pure "(..)"

infixOperator :: DefParser Definition
infixOperator =
  join $ (definition Function) <$> getLocation <*> inBraces operator

operator :: DefParser String
operator = many1 (oneOf "+-/*=.$<>:&|^?%#@~!")

-- I'm not interested in parsing arguments at this moment, except to know when the argument list is over.
arguments :: DefParser ()
arguments = do
  _ <-
    many $
    choice
      [ try whitespace1 >> pure 'x'
      , alphaNum
      , char '('
      , char ')'
      , char '{'
      , char '}'
      ]
  return ()

-- When using whitespace, always ensure it does not end us on the start of the next line.
-- This would indicate a new definition, which means we need to release control to the top level parser.
whitespace :: DefParser ()
whitespace = spaces >> notTopLevel

whitespace1 :: DefParser ()
whitespace1 = space >> whitespace

endOfFileOrLine :: DefParser ()
endOfFileOrLine = dump endOfLine <|> eof

comma :: DefParser ()
comma = whitespace >> char ',' >> whitespace

inBraces :: DefParser a -> DefParser a
inBraces parser = char '(' *> whitespace *> parser <* whitespace <* char ')'

inCurlyBraces :: DefParser a -> DefParser a
inCurlyBraces parser =
  char '{' *> whitespace *> parser <* whitespace <* char '}'

dump :: DefParser a -> DefParser ()
dump parser = parser *> pure ()

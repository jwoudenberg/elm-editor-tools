module ElmTools.ParseModule
  ( elmParser
  , parseString
  , findDefinition
  ) where

import Control.Monad (join)
import Data.List
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as Set
import ElmTools.Error
import ElmTools.ParseModule.Types
import Text.Parsec
import Text.Parsec.Indent

type DefParser result = IndentParser String ParseState result

data ParseState = ParseState
  { exportedNames :: ExposedNames
  }

findDefinition :: FilePath -> String -> IO (Either Error Definition)
findDefinition filePath name' = do
  parseResult <- elmParser filePath
  return $ do
    decs <- mapLeft (CouldNotParseElmModule filePath . show) parseResult
    let defs = mapMaybe getDef decs
    findDef name' defs

getDef :: Declaration -> Maybe Definition
getDef declaration =
  case declaration of
    Definition def -> Just def
    Import _ -> Nothing

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft fn either_ =
  case either_ of
    Left x -> Left (fn x)
    Right y -> Right y

findDef :: String -> [Definition] -> Either Error Definition
findDef name' defs =
  case find ((==) name' . name) defs of
    Nothing -> Left (CouldNotFindDefinition name')
    Just def -> Right def

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
  (option [] declaration) <* restOfLine
    -- I'd prefer not to have to `try` each definition parser here, but I don't see I have any choice.
    -- When a syntax error is encountered, I don't want to abort parsing, but continue on to the next definition.
    -- As far as I know, parsec offers no way to recover from parsers that failed after consuming input.
  where
    declaration = choice (fmap try declarationParsers)

declarationParsers :: [DefParser [Declaration]]
declarationParsers =
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
commentBlockEnd = eof <|> dump (string "-}")

moduleDefinition :: DefParser ()
moduleDefinition = do
  exportedNames' <-
    string "module" *> whitespace1 *> moduleName *> whitespace1 *>
    string "exposing" *>
    whitespace1 *>
    exposedList
  _ <- modifyState (\s -> s {exportedNames = exportedNames'})
  return ()

definition :: DefinitionType -> Location -> String -> DefParser Definition
definition definitionType' location' name' = do
  exportedNames' <- exportedNames <$> getState
  let scope' =
        case exportedNames' of
          All -> Exported
          Selected exported ->
            if Set.member name' exported
              then Exported
              else Global
  return $ DefinitionC definitionType' scope' location' name'

destructuredAssignment :: DefParser [Definition]
destructuredAssignment = destructuredContent <* whitespace <* char '='

destructuredContent :: DefParser [Definition]
destructuredContent =
  choice [destructuredRecord, destructuredTuple, pure <$> variable]

destructuredRecord :: DefParser [Definition]
destructuredRecord = inCurlyBraces $ sepBy variable (try comma)

destructuredTuple :: DefParser [Definition]
destructuredTuple = inBraces $ join <$> sepBy destructuredContent (try comma)

topFunction :: DefParser Definition
topFunction = infixOperator <|> variable <* whitespace <* arguments <* char '='

variable :: DefParser Definition
variable = join $ (definition Function) <$> getLocation <*> lowerCasedWord

sumType :: DefParser [Definition]
sumType = do
  typeKey *> typeDefinition *> equalSign *> constructors
  where
    typeKey = string "type" *> whitespace1
    equalSign = whitespace *> char '=' *> whitespace
    constructors =
      sepBy typeConstructor (try $ whitespace >> char '|' >> whitespace)

importStatement :: DefParser Import
importStatement =
  importKey *> (pure importDef) <*> moduleName <*> asClause <*> exposingClause
  where
    importKey = string "import" *> whitespace1
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
      string "type" >> whitespace1 >> string "alias" >> whitespace1

typeDefinition :: DefParser String
typeDefinition = do
  baseType <- capitalizedWord
  typeParams <-
    optionMaybe $ try $ whitespace1 >> sepBy lowerCasedWord whitespace1
  return $ concat $ intersperse " " $ baseType : (maybe [] id typeParams)

typeConstructor :: DefParser Definition
typeConstructor =
  join $
  (definition TypeConstructor) <$> getLocation <*> capitalizedWord <* arguments

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
  rest <- many letter
  return (initial : rest)

capitalizedWord :: DefParser String
capitalizedWord = do
  initial <- upper
  rest <- many letter
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
    exposedValue = exposedSumType <|> pure <$> lowerCasedWord

exposedSumType :: DefParser [String]
exposedSumType =
  choice
    [ try $ capitalizedWord *> whitespace *> exposedConstructors
    , try $ pure <$> (pure (++) <*> capitalizedWord <* whitespace <*> exposeAll)
    , pure <$> capitalizedWord
    ]
  where
    exposedConstructors = inBraces (sepBy capitalizedWord (try comma))

exposeAll :: DefParser String
exposeAll = inBraces (string "..") *> pure "(..)"

infixOperator :: DefParser Definition
infixOperator = inBraces operator

operator :: DefParser Definition
operator =
  join $
  (definition Function) <$> getLocation <*> many1 (oneOf "+-/*=.$<>:&|^?%#@~!")

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

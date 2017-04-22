{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Definitions
  ( elmParser
  , parseString
  , findDefinition
  , Location(..)
  , Definition(..)
  , Declaration(..)
  , Import(..)
  , Exposing(..)
  ) where

import Control.Monad (join)
import Data.Aeson
import Data.List
import qualified Data.Maybe
import Error
import Text.Parsec
import Text.Parsec.Indent

data Location = Location
  { fileName :: String
  , line :: Int
  , column :: Int
  } deriving (Show, Eq)

data Declaration
  = Definition Definition
  | Import Import
  deriving (Show, Eq)

data Definition
  = TopFunction String
                Location
  | TypeConstructor String
                    Location
  | TypeAlias String
              Location
  deriving (Show, Eq)

data Import = ImportC
  { qualifiedName :: String
  , localName :: String
  , exposing :: Exposing
  } deriving (Show, Eq)

data Exposing
  = All
  | Selected [String]
  deriving (Show, Eq)

instance ToJSON Definition where
  toJSON (TopFunction name location) =
    object
      [ "name" .= name
      , "fileName" .= fileName location
      , "line" .= line location
      , "column" .= column location
      ]
  toJSON (TypeConstructor name location) =
    object
      [ "name" .= name
      , "fileName" .= fileName location
      , "line" .= line location
      , "column" .= column location
      ]
  toJSON (TypeAlias name location) =
    object
      [ "name" .= name
      , "fileName" .= fileName location
      , "line" .= line location
      , "column" .= column location
      ]

type DefParser result = IndentParser String () result

findDefinition :: FilePath -> String -> IO (Either Error Definition)
findDefinition filePath name = do
  parseResult <- elmParser filePath
  return $ do
    decs <- mapLeft (CouldNotParseElmModule filePath . show) parseResult
    let defs = Data.Maybe.mapMaybe getDef decs
    findDef name defs

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
findDef name defs =
  case find ((==) name . defName) defs of
    Nothing -> Left (CouldNotFindDefinition name)
    Just def -> Right def

defName :: Definition -> String
defName def =
  case def of
    TopFunction name _ -> name
    TypeConstructor name _ -> name
    TypeAlias name _ -> name

elmParser :: FilePath -> IO (Either ParseError [Declaration])
elmParser filePath = do
  input <- readFile filePath
  return (parseString filePath input)

parseString :: FilePath -> String -> Either ParseError [Declaration]
parseString filePath fileContent =
  runIndentParser declarations () filePath fileContent

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
  [ pure <$> Import <$> import_
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
variable = (pure $ flip TopFunction) <*> getLocation <*> lowerCasedWord

sumType :: DefParser [Definition]
sumType = do
  _ <- string "type"
  _ <- whitespace1
  _ <- typeDefinition
  _ <- whitespace
  _ <- char '='
  _ <- whitespace
  typeConstructors <-
    sepBy typeConstructor (try $ whitespace >> char '|' >> whitespace)
  return typeConstructors

import_ :: DefParser Import
import_ = do
  _ <- string "import"
  _ <- whitespace1
  qualifiedName_ <- moduleName
  alias <-
    optionMaybe $
    try $ whitespace1 *> string "as" *> whitespace1 *> capitalizedWord
  let localName_ = maybe qualifiedName_ id alias
  maybeExposed <-
    optionMaybe $
    try $ whitespace1 *> string "exposing" *> whitespace1 *> exposing_
  let exposed = maybe (Selected []) id maybeExposed
  return $ ImportC qualifiedName_ localName_ exposed

typeAlias :: DefParser Definition
typeAlias = do
  _ <- typeAliasKeyword
  return (flip TypeAlias) <*> getLocation <*> capitalizedWord
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
  pure (flip TypeConstructor) <*> getLocation <*> capitalizedWord <* arguments

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

exposing_ :: DefParser Exposing
exposing_ =
  Selected <$>
  (inBraces $ sepBy (capitalizedWord <|> lowerCasedWord) (try comma))

infixOperator :: DefParser Definition
infixOperator = inBraces operator

operator :: DefParser Definition
operator =
  pure (flip TopFunction) <*> getLocation <*>
  many1 (oneOf "+-/*=.$<>:&|^?%#@~!")

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

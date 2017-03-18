{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Definitions
  ( elmParser
  , parseString
  , Location(..)
  , Definition(..)
  ) where

import Data.Aeson
import Data.List
import Text.Parsec
import Text.Parsec.Indent

data Location = Location
  { fileName :: String
  , line :: Int
  , column :: Int
  } deriving (Show, Eq)

data Definition
  = TopFunction String
                Location
  | TypeConstructor String
                    Location
  | TypeAlias String
              Location
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

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName_ = do
  input <- readFile fileName_
  return (parseString fileName_ input)

parseString :: String -> String -> Either ParseError [Definition]
parseString fileName_ fileContent =
  runIndentParser definitions () fileName_ fileContent

definitions :: DefParser [Definition]
definitions = do
  topLevel
  result <- mconcat <$> manyTill line_ eof
  return result

line_ :: DefParser [Definition]
line_ = do
  (option [] declaration) <* restOfLine
    -- I'd prefer not to have to `try` each definition parser here, but I don't see I have any choice.
    -- When a syntax error is encountered, I don't want to abort parsing, but continue on to the next definition.
    -- As far as I know, parsec offers no way to recover from parsers that failed after consuming input.
  where
    declaration = choice (fmap try declarationParsers)

declarationParsers :: [DefParser [Definition]]
declarationParsers =
  [pure <$> typeAlias, sumType, destructuredAssignment, pure <$> topFunction]

restOfLine :: DefParser String
restOfLine = manyTill anyChar endOfFileOrLine

destructuredAssignment :: DefParser [Definition]
destructuredAssignment = destructuredRecord <|> destructuredTuple

destructuredRecord :: DefParser [Definition]
destructuredRecord = do
  _ <- char '{'
  _ <- whitespace
  recordDefinitions <-
    sepBy variable (try $ whitespace >> char ',' >> whitespace)
  _ <- whitespace
  _ <- char '}'
  _ <- whitespace
  _ <- char '='
  return recordDefinitions

destructuredTuple :: DefParser [Definition]
destructuredTuple = do
  _ <- char '('
  _ <- whitespace
  recordDefinitions <-
    sepBy variable (try $ whitespace >> char ',' >> whitespace)
  _ <- whitespace
  _ <- char ')'
  _ <- whitespace
  _ <- char '='
  return recordDefinitions

topFunction :: DefParser Definition
topFunction = do
  definition <- infixOperator <|> variable
  _ <- whitespace
  _ <-
    many $ (try whitespace1 >> pure 'x') <|> alphaNum <|> char '(' <|> char ')'
  _ <- char '='
  return definition

variable :: DefParser Definition
variable = (pure $ flip TopFunction) <*> getLocation <*> lowerCasedWord

sumType :: DefParser [Definition]
sumType = do
  _ <- string "type"
  _ <- whitespace1 >> notTopLevel
  _ <- typeDefinition
  _ <- whitespace
  _ <- char '='
  _ <- whitespace
  typeConstructors <-
    sepBy typeConstructor (try $ whitespace >> char '|' >> whitespace)
  return typeConstructors

typeAlias :: DefParser Definition
typeAlias = do
  location <- getLocation
  _ <- string "type"
  _ <- whitespace1
  _ <- string "alias"
  _ <- whitespace1
  name <- capitalizedWord
  return (TypeAlias name location)

typeDefinition :: DefParser String
typeDefinition = do
  baseType <- capitalizedWord
  typeParams <-
    optionMaybe $ try $ whitespace1 >> sepBy lowerCasedWord whitespace1
  return $ concat $ intersperse " " $ baseType : (maybe [] id typeParams)

typeConstructor :: DefParser Definition
typeConstructor = do
  location <- getLocation
  name <- capitalizedWord
  _ <-
    many $ (try whitespace1 >> pure 'x') <|> alphaNum <|> char '(' <|> char ')'
  return (TypeConstructor name location)

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

infixOperator :: DefParser Definition
infixOperator = do
  location <- getLocation
  _ <- char '('
  _ <- whitespace
  name <- many1 (oneOf "+-/*=.$<>:&|^?%#@~!")
  _ <- whitespace
  _ <- char ')'
  return (TopFunction name location)

-- When using whitespace, always ensure it does not end us on the start of the next line.
-- This would indicate a new definition, which means we need to release control to the top level parser.
whitespace :: DefParser ()
whitespace = spaces >> notTopLevel

whitespace1 :: DefParser ()
whitespace1 = space >> whitespace

endOfFileOrLine :: DefParser ()
endOfFileOrLine = (endOfLine >> pure ()) <|> eof

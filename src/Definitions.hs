{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Definitions
  ( elmParser
  , parseString
  , Location(..)
  , Definition(..)
  ) where

import Control.Monad (join)
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

elmParser :: FilePath -> IO (Either ParseError [Definition])
elmParser fileName_ = do
  input <- readFile fileName_
  return (parseString fileName_ input)

parseString :: FilePath -> String -> Either ParseError [Definition]
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

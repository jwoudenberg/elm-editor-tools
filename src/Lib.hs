{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( elmParser
    , parseString
    , Location(..)
    , Definition(..)
    ) where

import Data.Aeson
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Generics
import Text.Parsec.String
import Text.ParserCombinators.Parsec

data Location = Location
    { fileName :: String
    , line :: Int
    , column :: Int
    } deriving (Show, Eq)

data Definition =
    Definition String
               Location
    deriving (Show, Eq)

instance ToJSON Definition where
    toJSON (Definition name location) =
        object
            [ "name" .= name
            , "fileName" .= fileName location
            , "line" .= line location
            , "column" .= column location
            ]

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName = parseFromFile definitions fileName

parseString :: String -> String -> Either ParseError [Definition]
parseString fileName fileContent = parse definitions fileName fileContent

definitions :: GenParser Char st [Definition]
definitions = do
    result <- catMaybes <$> manyTill line_ eof
    return result

line_ :: GenParser Char st (Maybe Definition)
line_ = do
    spaces
    choice [Just <$> try definition, anyLine >> return Nothing]

anyLine :: GenParser Char st String
anyLine = do
    manyTill anyChar (newline <|> (eof >> return 'x'))

definition :: GenParser Char st Definition
definition = do
    location <- toLocation <$> getPosition
    name <- operator <|> lowerCasedWord
    spaces
    char ':'
    return (Definition name location)

toLocation :: SourcePos -> Location
toLocation sourcePos =
    Location
    { fileName = sourceName sourcePos
    , line = sourceLine sourcePos
    , column = sourceColumn sourcePos
    }

lowerCasedWord :: GenParser Char st String
lowerCasedWord = do
    initial <- lower
    rest <- many letter
    return (initial : rest)

operator :: GenParser Char st String
operator = do
    char '('
    spaces
    op <- many1 (noneOf ")")
    spaces
    char ')'
    return op

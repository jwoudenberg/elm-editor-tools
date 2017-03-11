{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( elmParser
    , parseString
    , Location(..)
    , Definition(..)
    ) where

import Data.Aeson
import Text.Parsec.String
import Text.ParserCombinators.Parsec

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

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName_ = parseFromFile definitions fileName_

parseString :: String -> String -> Either ParseError [Definition]
parseString fileName_ fileContent = parse definitions fileName_ fileContent

definitions :: GenParser Char st [Definition]
definitions = do
    result <- mconcat <$> manyTill line_ eof
    return result

line_ :: GenParser Char st [Definition]
line_ = do
    spaces
    choice [pure <$> try topFunction, anyLine >> return []]

anyLine :: GenParser Char st String
anyLine = manyTill anyChar (newline <|> (eof >> return 'x'))

topFunction :: GenParser Char st Definition
topFunction = do
    location <- toLocation <$> getPosition
    name <- operator <|> lowerCasedWord
    spaces
    _ <- char ':'
    return (TopFunction name location)

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
    _ <- char '('
    spaces
    op <- many1 (noneOf ")")
    spaces
    _ <- char ')'
    return op

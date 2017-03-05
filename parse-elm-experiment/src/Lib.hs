module Lib
    ( elmParser
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Data.Maybe
import Data.List
import Debug.Trace

data Definition = Definition String deriving (Show)

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName =
    parseFromFile definitions fileName

definitions :: GenParser Char st [Definition]
definitions = do
    result <- catMaybes <$> many line
    eof
    return result

line :: GenParser Char st (Maybe Definition)
line = do
    choice
        [ Just <$> try definition
        , do
            anyLine
            return Nothing
        ]

anyLine :: GenParser Char st String
anyLine = do
    manyTill anyChar newline

definition :: GenParser Char st Definition
definition = do
    name <- operator <|> lowerCasedWord
    spaces
    char ':'
    return (Definition name)

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

module Lib
    ( elmParser
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Data.Maybe
import Data.List
import Debug.Trace

data Definition = Definition
    { name :: String
    , fileName :: String
    , line :: Int
    , column :: Int
    }
    deriving (Show)

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName =
    parseFromFile definitions fileName

definitions :: GenParser Char st [Definition]
definitions = do
    result <- catMaybes <$> many line_
    eof
    return result

line_ :: GenParser Char st (Maybe Definition)
line_ = do
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
    sourcePos <- getPosition
    name <- operator <|> lowerCasedWord
    spaces
    char ':'
    return (Definition
        { name = name
        , fileName = sourceName sourcePos
        , line = sourceLine sourcePos
        , column = sourceColumn sourcePos
        })

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

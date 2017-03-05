module Lib
    ( elmParser
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Data.Maybe
import Data.List
import Debug.Trace

data Info
    = Import String
    | Definition String
    deriving (Show)

elmParser :: String -> IO (Either ParseError [Info])
elmParser fileName =
    parseFromFile umports fileName

umports :: GenParser Char st [Info]
umports = do
    result <- catMaybes <$> many line
    eof
    return result

line :: GenParser Char st (Maybe Info)
line = do
    choice
        [ Just <$> try umport
        , Just <$> try definition
        , do
            anyLine
            return Nothing
        ]

anyLine :: GenParser Char st String
anyLine = do
    manyTill anyChar newline

umport :: GenParser Char st Info
umport = do
    string "import"
    spaces
    name <- moduleName
    spaces
    return (Import name)

moduleName :: GenParser Char st String
moduleName = do
    manyTill anyChar space

definition :: GenParser Char st Info
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

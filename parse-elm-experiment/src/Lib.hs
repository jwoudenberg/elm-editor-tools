module Lib
    ( elmParser
    ) where

import Text.ParserCombinators.Parsec
import Text.Parsec.String
import Data.Maybe

type Import = String

elmParser :: String -> IO (Either ParseError [Import])
elmParser fileName =
    parseFromFile umports fileName

umports :: GenParser Char st [Import]
umports = do
    result <- catMaybes <$> many line
    eof
    return result


line :: GenParser Char st (Maybe Import)
line = do
    choice
        [ Just <$> try umport
        , do
            anyLine
            return Nothing
        ]

anyLine :: GenParser Char st String
anyLine = do
    line <- many (noneOf "\n")
    newline
    return line

umport :: GenParser Char st Import
umport = do
    string "import"
    spaces
    name <- moduleName
    spaces
    return name

moduleName :: GenParser Char st String
moduleName = do
    manyTill anyChar space

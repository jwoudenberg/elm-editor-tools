{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( elmParser
    , parseString
    , Location(..)
    , Definition(..)
    ) where

import Data.Aeson
import Data.List
import Text.Parsec
import Text.Parsec.String

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

type DefParser result = GenParser Char () result

elmParser :: String -> IO (Either ParseError [Definition])
elmParser fileName_ = parseFromFile definitions fileName_

parseString :: String -> String -> Either ParseError [Definition]
parseString fileName_ fileContent = parse definitions fileName_ fileContent

definitions :: DefParser [Definition]
definitions = do
    result <- mconcat <$> manyTill line_ eof
    return result

line_ :: DefParser [Definition]
line_ = do
    choice [pure <$> try topFunction, try sumType, anyLine >> return []]

anyLine :: DefParser String
anyLine = manyTill anyChar (endOfLine <|> (eof >> return 'x'))

topFunction :: DefParser Definition
topFunction = do
    location <- getLocation
    name <- operator <|> lowerCasedWord
    spaces
    _ <- char ':'
    return (TopFunction name location)

sumType :: DefParser [Definition]
sumType = do
    _ <- string "type"
    spaces1
    _ <- typeDefinition
    spaces
    _ <- char '='
    spaces
    sepBy typeConstructor (spaces >> char '|' >> spaces)

typeDefinition :: DefParser String
typeDefinition = do
    baseType <- capitalizedWord
    typeParams <- optionMaybe $ try $ spaces1 >> sepBy lowerCasedWord spaces1
    return $ concat $ intersperse " " $ baseType : (maybe [] id typeParams)

typeConstructor :: DefParser Definition
typeConstructor = do
    location <- getLocation
    name <- capitalizedWord
    optional $ spaces1 >> sepBy lowerCasedWord spaces1
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

operator :: DefParser String
operator = do
    _ <- char '('
    spaces
    op <- many1 (noneOf ")")
    spaces
    _ <- char ')'
    return op

spaces1 :: DefParser ()
spaces1 = space >> spaces >> pure ()

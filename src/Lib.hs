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
    choice [pure <$> try topFunction, try sumType, anyLine >> return []]

anyLine :: GenParser Char st String
anyLine = manyTill anyChar (endOfLine <|> (eof >> return 'x'))

topFunction :: GenParser Char st Definition
topFunction = do
    location <- getLocation
    name <- operator <|> lowerCasedWord
    spaces
    _ <- char ':'
    return (TopFunction name location)

sumType :: GenParser Char st [Definition]
sumType = do
    _ <- string "type"
    spaces1
    _ <- typeDefinition
    spaces
    _ <- char '='
    spaces
    sepBy typeConstructor (spaces >> char '|' >> spaces)

typeDefinition :: GenParser Char st String
typeDefinition = do
    baseType <- capitalizedWord
    typeParams <- optionMaybe $ try $ spaces1 >> sepBy lowerCasedWord spaces1
    return $ concat $ intersperse " " $ baseType : (maybe [] id typeParams)

typeConstructor :: GenParser Char st Definition
typeConstructor = do
    location <- getLocation
    name <- capitalizedWord
    optional $ spaces1 >> sepBy lowerCasedWord spaces1
    return (TypeConstructor name location)

getLocation :: GenParser Char st Location
getLocation = do
    sourcePos <- getPosition
    pure $
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

capitalizedWord :: GenParser Char st String
capitalizedWord = do
    initial <- upper
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

spaces1 :: GenParser Char st ()
spaces1 = space >> spaces >> pure ()

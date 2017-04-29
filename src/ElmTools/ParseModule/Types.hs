{-# LANGUAGE OverloadedStrings #-}

module ElmTools.ParseModule.Types where

import Data.Aeson

data Location = Location
  { fileName :: String
  , line :: Int
  , column :: Int
  } deriving (Show, Eq)

data Declaration
  = Definition Definition
  | Import Import
  deriving (Show, Eq)

data Definition
  = TopFunction String
                Location
  | TypeConstructor String
                    Location
  | TypeAlias String
              Location
  deriving (Show, Eq)

data Import = ImportC
  { qualifiedName :: String
  , localName :: String
  , exposedNames :: ExposedNames
  } deriving (Show, Eq)

data ExposedNames
  = All
  | Selected [String]
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

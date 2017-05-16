{-# LANGUAGE OverloadedStrings #-}

module ElmTools.ParseModule.Types where

import Data.Aeson
import Data.Set (Set)

data Declaration
  = Definition Definition
  | Import Import
  deriving (Show, Eq)

data Definition = DefinitionC
  { definitionType :: DefinitionType
  , scope :: Scope
  , location :: Location
  , name :: String
  } deriving (Show, Eq)

data DefinitionType
  = Function
  | TypeConstructor String
  | TypeAlias
  deriving (Show, Eq)

data Scope
  = Exported
  | Global
  deriving (Show, Eq)

data Location = Location
  { fileName :: String
  , line :: Int
  , column :: Int
  } deriving (Show, Eq)

instance ToJSON Definition where
  toJSON (DefinitionC _ _ location' name') =
    object
      [ "name" .= name'
      , "fileName" .= fileName location'
      , "line" .= line location'
      , "column" .= column location'
      ]

data Import = ImportC
  { qualifiedName :: String
  , localName :: String
  , exposedNames :: ExposedNames
  } deriving (Show, Eq)

data ExposedNames
  = All
  | Selected (Set ExposedName)
  deriving (Show, Eq)

data ExposedName
  = TipeWithConstructors String
  | TipeWithSingleConstructor String
                              String
  | Tipe String
  | NonTipe String
  deriving (Show, Eq, Ord)

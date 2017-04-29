module ElmTools.Error
  ( Error(..)
  ) where

data Error
  = CouldNotFindElmJSON
  | CouldNotParseElmJSON FilePath
  | CouldNotParseDepsJSON FilePath
  | CouldNotFindModule
  | CouldNotParseElmModule FilePath
                           String
  | CouldNotFindDefinition String

instance Show (Error) where
  show CouldNotFindElmJSON = "No elm-package.Rson file found."
  show (CouldNotParseElmJSON elmJSONPath) =
    "Could not parse elm-package.json found at: " ++ elmJSONPath
  show (CouldNotParseDepsJSON depsJSONPath) =
    "Could not parse exact-dependencies.json found at: " ++ depsJSONPath
  show CouldNotFindModule = "Could not find module."
  show (CouldNotParseElmModule elmModulePath parseError) =
    "Could not parse elm module: " ++
    elmModulePath ++ ". Parse error: " ++ parseError
  show (CouldNotFindDefinition name) = "Could not find definition for: " ++ name

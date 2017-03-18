{-# OPTIONS_GHC -Wall #-}

module Error
  ( Error(..)
  ) where

data Error
  = CouldNotFindElmJSON
  | CouldNotParseElmJSON FilePath
  | CouldNotParseDepsJSON FilePath
  | CouldNotFindModule

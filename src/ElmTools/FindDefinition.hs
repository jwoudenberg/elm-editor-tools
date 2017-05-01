module ElmTools.FindDefinition
  ( findDefinition
  ) where

import Data.List (find)
import Data.Maybe (mapMaybe)
import ElmTools.Error
import qualified ElmTools.ParseModule as ParseModule
import ElmTools.ParseModule.Types

findDefinition :: FilePath -> String -> IO (Either Error Definition)
findDefinition filePath name' = do
  parseResult <- ParseModule.elmParser filePath
  return $ do
    decs <- mapLeft (CouldNotParseElmModule filePath . show) parseResult
    let defs = mapMaybe getDef decs
    findDef name' defs

findDef :: String -> [Definition] -> Either Error Definition
findDef name' defs =
  case find ((==) name' . name) defs of
    Nothing -> Left (CouldNotFindDefinition name')
    Just def -> Right def

getDef :: Declaration -> Maybe Definition
getDef declaration =
  case declaration of
    Definition def -> Just def
    Import _ -> Nothing

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft fn either_ =
  case either_ of
    Left x -> Left (fn x)
    Right y -> Right y

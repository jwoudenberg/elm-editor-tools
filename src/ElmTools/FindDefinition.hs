module ElmTools.FindDefinition
  ( findDefinition
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Data.Foldable (asum)
import Data.List (find)
import Data.Maybe (mapMaybe)
import ElmTools.Error
import qualified ElmTools.ParseModule as ParseModule
import ElmTools.ParseModule.Types
import ElmTools.ResolveModule (resolveModule)

type App a = ExceptT Error IO a

-- TODO:
-- - [x] Search through local file and if that fails all imports
-- - [ ] If an import exposes the right name, only search that one.
-- - [ ] Skip searching imports not exposing everything or the right name.
-- - [ ] Handle qualified imports.
findDefinition :: FilePath -> String -> IO (Either Error Definition)
findDefinition filePath name' =
  runExceptT $ do
    declarations <- parseElmModule' filePath
    let imports = mapMaybe getImport declarations
    let hit = definitionFromDeclarations name' declarations
    -- TODO: evalaute hitInImports lazily
    hitInImports <- asum <$> traverse (findInImport filePath name') imports
    case hit <|> hitInImports of
      Nothing -> throwError (CouldNotFindDefinition name')
      Just definition -> return definition

parseElmModule' :: FilePath -> App [Declaration]
parseElmModule' filePath =
  ExceptT $ do
    parseResult <- ParseModule.elmParser filePath
    return $ mapLeft toError parseResult
  where
    toError = CouldNotParseElmModule filePath . show

-- TODO: implement this
findInImport :: FilePath -> String -> Import -> App (Maybe Definition)
findInImport importedFromFile name' import' = do
  modulePath <- ExceptT $ resolveModule importedFromFile (qualifiedName import')
  declarations <- parseElmModule' modulePath
  return $ definitionFromDeclarations name' declarations

definitionFromDeclarations :: String -> [Declaration] -> Maybe Definition
definitionFromDeclarations name' declarations =
  find ((==) name' . name) definitions
  where
    definitions :: [Definition]
    definitions = mapMaybe getDefinition declarations

getDefinition :: Declaration -> Maybe Definition
getDefinition declaration =
  case declaration of
    Definition definition -> Just definition
    Import _ -> Nothing

getImport :: Declaration -> Maybe Import
getImport declaration =
  case declaration of
    Definition _ -> Nothing
    Import import' -> Just import'

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft fn either_ =
  case either_ of
    Left x -> Left (fn x)
    Right y -> Right y

module Parser
    where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
-- import Text.XML.Light

import Control.Applicative
import Control.Monad

data XMLTagType = TagOpen
                | TagClosed
                | TagBoth

instance Functor ParseResult where
  fmap f (ParseOk x)           = ParseOk $ f x
  fmap f (ParseFailed loc msg) = ParseFailed loc msg

instance Applicative ParseResult where
  pure = ParseOk
  ParseOk f           <*> x = f <$> x
  ParseFailed loc msg <*> _ = ParseFailed loc msg

instance Monad ParseResult where
  return = ParseOk
  ParseOk x           >>= f = f x
  ParseFailed loc msg >>= _ = ParseFailed loc msg

noValue :: String
noValue = []

noAttr :: [(String, ())]
noAttr = []

traverseModule :: HsModule -> String
traverseModule (HsModule srcLoc mod _ _ decl) =
   xmlNode "Location" (traverseSrcLoc srcLoc) noAttr ++
   xmlNode "Module" (traverseMod mod) noAttr
--    show decl


traverseSrcLoc :: SrcLoc -> String
traverseSrcLoc (SrcLoc _ line column) =
    xmlNode "SrcLoc" noValue
                [
                 ("srcLine",   line),
                 ("srcColumn", column)
                ]

traverseMod :: Module -> String
traverseMod (Module mod) = mod

traverseHsDecl :: HsDecl -> String
traverseHsDecl (HsTypeDecl srcLoc hsName hsNameList hsType) = 
    traverseSrcLoc srcLoc

traverseHsDecl (HsDataDecl srcLoc hsContext hsName hsNameList hsConDeclList hsQNameList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsInfixDecl srcLoc hsAssoc int hsOpList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNameList hsConDecl hsQNameList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsClassDecl srcLoc hsContext hsName hsNameList hsDeclList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsInstDecl srcLoc hsContext hsQName hsTypeList hsDeclList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsDefaultDecl srcLoc hsTypeList) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsTypeSig srcLoc hsNameList hsQualType) =
    traverseSrcLoc srcLoc

traverseHsDecl (HsFunBind hsMatchlist) = undefined

traverseHsDecl (HsPatBind srcLoc hsPat hsRhs hsDeclList) =
    traverseSrcLoc srcLoc

-- currently unused
traverseHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = undefined
traverseHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = undefined

traverseHsName :: HsName -> String
traverseHsName (HsIdent  value) = xmlNode "HsIdent"  value noAttr
traverseHsName (HsSymbol value) = xmlNode "HsSymbol" value noAttr

trimIndent :: String -> String
trimIndent [] = []
trimIndent xs = ' ' : xs

xmlTag :: XMLTagType -> String -> String
xmlTag TagOpen   value = "<"  ++ value ++ ">"
xmlTag TagClosed value = "</" ++ value ++ ">"
xmlTag TagBoth   value = "<"  ++ value ++ "/>"

xmlNode :: (Show a) => String -> String -> [(String, a)] -> String
xmlNode name [] attrList =
    xmlTag TagBoth (name ++ trimIndent (attrList >>= xmlAttr))

xmlNode name value attrList = 
    xmlTag TagOpen (name ++ trimIndent (attrList >>= xmlAttr)) ++
    value ++
    xmlTag TagClosed name

xmlAttr :: (Show a) => (String, a) -> String
xmlAttr (name, value) = name ++ " = \"" ++ show value ++ "\" "

-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]


main :: IO ()
main = do
  contents <- readFile "./Crash.hs"
  print $ traverseModule <$> parseModule contents


--  print $ parseModule contents
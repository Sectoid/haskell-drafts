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
   xmlNode "Module" (traverseMod mod) noAttr ++
   xmlNode "Body" (decl >>= traverseHsDecl) noAttr


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
    xmlNode "HsTypeDecl" (traverseHsName hsName) noAttr

traverseHsDecl (HsDataDecl srcLoc hsContext hsName hsNameList hsConDeclList hsQNameList) =
    xmlNode "HsDataDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsInfixDecl srcLoc hsAssoc int hsOpList) =
    xmlNode "HsInfixDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNameList hsConDecl hsQNameList) =
    xmlNode "HsNewTypeDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsClassDecl srcLoc hsContext hsName hsNameList hsDeclList) =
    xmlNode "HsClassDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsInstDecl srcLoc hsContext hsQName hsTypeList hsDeclList) =
    xmlNode "HsInstDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsDefaultDecl srcLoc hsTypeList) =
    xmlNode "HsDefaultDecl" (traverseSrcLoc srcLoc) noAttr

traverseHsDecl (HsTypeSig srcLoc hsNameList hsQualType) =
    xmlNode "HsTypeSig" (traverseHsNameList hsNameList) noAttr

traverseHsDecl (HsFunBind hsMatchlist) =
    xmlNode "HsFunBind" noValue noAttr

traverseHsDecl (HsPatBind srcLoc hsPat hsRhs hsDeclList) =
    xmlNode "HsPatBind" (traverseSrcLoc srcLoc) noAttr

-- currently unused
traverseHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = undefined
traverseHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = undefined

traverseHsNameList :: [HsName] -> String
traverseHsNameList hsNames = xmlNode "Names" (hsNames >>= traverseHsName) noAttr

-- we currently do not differ these two
traverseHsName :: HsName -> String
traverseHsName (HsIdent  value) = xmlNode "HsName" value noAttr
traverseHsName (HsSymbol value) = xmlNode "HsName" value noAttr

trimIndent :: String -> String
trimIndent [] = []
trimIndent xs = ' ' : xs

xmlTag :: XMLTagType -> String -> String
xmlTag TagOpen   value = '<'  : value ++ ">"
xmlTag TagClosed value = "</" ++ value ++ ">"
xmlTag TagBoth   value = '<'  : value ++ "/>"

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
module Parser
    where

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

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

noValue :: String
noValue = []

noAttr :: [(String, String)]
noAttr = []

traverseModule :: HsModule -> String
traverseModule (HsModule srcLoc mod _ _ decl) =
   xmlNode "HsModule" 
               (
                xmlNode "Location" (traverseSrcLoc srcLoc)   noAttr ++
                xmlNode "Module"   (traverseMod mod)         noAttr ++
                xmlNode "Body"     (decl >>= traverseHsDecl) noAttr
               ) 
               [
                ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
                ("xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
               ]

traverseSrcLoc :: SrcLoc -> String
traverseSrcLoc (SrcLoc _ line column) =
    xmlNode "SrcLoc" noValue
                [
                 ("srcLine",   show line),
                 ("srcColumn", show column)
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
    xmlNode "HsTypeSig" (traverseHsNameList hsNameList ++ traverseHsQualType hsQualType) noAttr

traverseHsDecl (HsFunBind hsMatchlist) =
    xmlNode "HsFunBind" noValue noAttr

traverseHsDecl (HsPatBind srcLoc hsPat hsRhs hsDeclList) =
    xmlNode "HsPatBind" (traverseSrcLoc srcLoc) noAttr

-- currently unused
traverseHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = undefined
traverseHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = undefined

traverseHsNameList :: [HsName] -> String
traverseHsNameList hsNames = xmlNode "Names" (hsNames >>= traverseHsName) noAttr

traverseHsQualType :: HsQualType -> String
traverseHsQualType (HsQualType context typ) =
    xmlNode "Type" (
                    xmlNode "Context" (traverseHsAsstList context) noAttr ++
                    xmlNode "Type"    (traverseHsType typ) [("xsi:type", showHsType typ)]
                   ) noAttr

traverseHsType :: HsType -> String
traverseHsType (HsTyFun inType outType) = undefined
traverseHsType (HsTyTuple typeList) = undefined

traverseHsType (HsTyApp first second) =
    xmlNode "First"  (traverseHsType first)  [("xsi:type", showHsType first)] ++
    xmlNode "Second" (traverseHsType second) [("xsi:type", showHsType second)]

traverseHsType (HsTyVar name) = undefined
traverseHsType (HsTyCon qName) =
    xmlNode "Name" (traverseHsQName qName) [("xsi:type", showHsQName qName)]

traverseHsQName :: HsQName -> String
traverseHsQName (Qual mod name) = undefined
traverseHsQName (UnQual name)   = strip name
    where strip (HsIdent  value) = value
          strip (HsSymbol value) = value
traverseHsQName (Special con)   =
    xmlNode "Value" noValue [("xsi:type", showHsSpecialCon con)]

showHsType :: HsType -> String
showHsType (HsTyFun _ _)        = "HsTyFun"
showHsType (HsTyTuple _)        = "HsTyTuple"
showHsType (HsTyApp _ _)        = "HsTyApp"
showHsType (HsTyVar _)          = "HsTyVar"
showHsType (HsTyCon _)          = "HsTyCon"

showHsQName :: HsQName -> String
showHsQName (Qual _ _)          = "Qual"
showHsQName (UnQual _)          = "UnQual"
showHsQName (Special _)         = "Special"

showHsSpecialCon :: HsSpecialCon -> String
showHsSpecialCon HsUnitCon      = "HsUnitCon"
showHsSpecialCon HsListCon      = "HsListCon"
showHsSpecialCon HsFunCon       = "HsFunCon"
showHsSpecialCon (HsTupleCon _) = "HsTupleCon"
showHsSpecialCon HsCons         = "HsCons"

-- todo
traverseHsAsst :: HsAsst -> String
traverseHsAsst asst = undefined

traverseHsAsstList :: [HsAsst] -> String
traverseHsAsstList assts =
    xmlNode "Assertions" (assts >>= traverseHsAsst) noAttr

-- we currently do not differ these two
traverseHsName :: HsName -> String
traverseHsName (HsIdent  value) = xmlNode "HsName" value noAttr
traverseHsName (HsSymbol value) = xmlNode "HsName" value noAttr

trimIndent :: String -> String
trimIndent [] = []
trimIndent xs = ' ' : xs

xmlTag :: XMLTagType -> String -> String
xmlTag TagOpen   value = '<'       : value ++ ">"
xmlTag TagClosed value = '<' : '/' : value ++ ">"
xmlTag TagBoth   value = '<'       : value ++ "/>"

xmlNode :: String -> String -> [(String, String)] -> String
xmlNode name [] attrList =
    xmlTag TagBoth (name ++ trimIndent (attrList >>= xmlAttr))

xmlNode name value attrList = 
    xmlTag TagOpen (name ++ trimIndent (attrList >>= xmlAttr)) ++
    value ++
    xmlTag TagClosed name

xmlAttr :: (String, String) -> String
xmlAttr (name, value) = name ++ "=\"" ++ value ++ "\" "

-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]


main :: IO ()
main = do
  contents <- readFile "./Crash.hs"
  print $ traverseModule <$> parseModule contents


--  print $ parseModule contents
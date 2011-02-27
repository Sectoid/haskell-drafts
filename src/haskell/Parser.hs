module Main
    where

import System.Environment

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

import Control.Applicative
import Control.Monad

import Show
import XML

noValue :: String
noValue = []

noAttr :: [(String, String)]
noAttr = []

stripResult :: ParseResult HsModule -> HsModule
stripResult (ParseOk m)       = m
stripResult (ParseFailed _ _) = undefined

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

traverseHsDecl (HsPatBind srcLoc pat rhs hsDeclList) =
    xmlNode "HsPatBind" 
                (
                 xmlNode "Pattern" (traverseHsPat pat) [xsiType $ showHsPat pat] ++
                 xmlNode "Rhs" (traverseHsRhs rhs) [xsiType $ showHsRhs rhs]
                ) noAttr

-- currently unused
traverseHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = undefined
traverseHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = undefined

traverseHsPat :: HsPat -> String
traverseHsPat (HsPVar name)       = xmlNode "Name" (stripHsName name) noAttr
traverseHsPat (HsPLit _)          = "HsPLit"
traverseHsPat (HsPNeg _)          = "HsPNeg"
traverseHsPat (HsPInfixApp _ _ _) = "HsPInfixApp"
traverseHsPat (HsPApp _ _)        = "HsPApp"
traverseHsPat (HsPTuple _)        = "HsPTuple"
traverseHsPat (HsPList _)         = "HsPList"
traverseHsPat (HsPParen _)        = "HsPParen"
traverseHsPat (HsPRec _ _)        = "HsPRec"
traverseHsPat (HsPAsPat _ _)      = "HsPAsPat"
traverseHsPat (HsPWildCard)       = "HsPWildCard"
traverseHsPat (HsPIrrPat _)       = "HsPIrrPat"

traverseHsRhs :: HsRhs -> String
traverseHsRhs (HsUnGuardedRhs exp)  =
    xmlNode "Expression" (traverseHsExp exp) [xsiType $ showHsExp exp]
traverseHsRhs (HsGuardedRhss _)   = "HsGuardedRhss"

traverseHsExp :: HsExp -> String
traverseHsExp (HsVar name) =
    xmlNode "Name" (traverseHsQName name) [xsiType $ showHsQName name]
traverseHsExp (HsCon _)                = "HsCon"
traverseHsExp (HsLit lit) =
    xmlNode "Value" (traverseHsLiteral lit) [xsiType $ showHsLiteral lit]
traverseHsExp (HsInfixApp lhs op rhs) =
    xmlNode "Left"  (traverseHsExp lhs) [xsiType $ showHsExp lhs] ++
    xmlNode "Op"    (traverseHsQOp op)  [xsiType $ showHsQOp op]  ++
    xmlNode "Right" (traverseHsExp rhs) [xsiType $ showHsExp rhs]

traverseHsExp (HsApp lhs rhs) =
    xmlNode "Left"  (traverseHsExp lhs) [xsiType $ showHsExp lhs] ++
    xmlNode "Right" (traverseHsExp rhs) [xsiType $ showHsExp rhs]
traverseHsExp (HsNegApp _)             = "HsNegApp"
traverseHsExp (HsLambda _ _ _)         = "HsLambda"
traverseHsExp (HsLet _ _)              = "HsLet"
traverseHsExp (HsIf _ _ _)             = "HsIf"
traverseHsExp (HsCase _ _)             = "HsCase"
traverseHsExp (HsDo _)                 = "HsDo"
traverseHsExp (HsTuple _)              = "HsTuple"
traverseHsExp (HsList _)               = "HsList"
traverseHsExp (HsParen _)              = "HsParen"
traverseHsExp (HsLeftSection _ _)      = "HsLeftSection"
traverseHsExp (HsRightSection op rhs) =
    xmlNode "Op"    (traverseHsQOp op)  [xsiType $ showHsQOp op] ++
    xmlNode "Right" (traverseHsExp rhs) [xsiType $ showHsExp rhs]
traverseHsExp (HsRecConstr _ _)        = "HsRecConstr"
traverseHsExp (HsRecUpdate _ _)        = "HsRecUpdate"
traverseHsExp (HsEnumFrom _)           = "HsEnumFrom"
traverseHsExp (HsEnumFromTo from to) =
    xmlNode "From" (traverseHsExp from) [xsiType $ showHsExp from] ++
    xmlNode "To"   (traverseHsExp to)   [xsiType $ showHsExp to]
traverseHsExp (HsEnumFromThen _ _)     = "HsEnumFromThen"
traverseHsExp (HsEnumFromThenTo _ _ _) = "HsEnumFromThenTo"
traverseHsExp (HsListComp _ _)         = "HsListComp"
traverseHsExp (HsExpTypeSig _ _ _)     = "HsExpTypeSig"
traverseHsExp (HsAsPat _ _)            = "HsAsPat"
traverseHsExp (HsWildCard)             = "HsWildCard"
traverseHsExp (HsIrrPat _)             = "HsIrrPat"

traverseHsNameList :: [HsName] -> String
traverseHsNameList hsNames = xmlNode "Names" (hsNames >>= traverseHsName) noAttr

traverseHsQualType :: HsQualType -> String
traverseHsQualType (HsQualType context typ) =
    xmlNode "Type" 
                (
                 xmlNode "Context" (traverseHsAsstList context) noAttr ++
                 xmlNode "Type"    (traverseHsType typ) [xsiType $ showHsType typ]
                ) noAttr

traverseHsType :: HsType -> String
traverseHsType (HsTyFun inType outType) = undefined
traverseHsType (HsTyTuple typeList) = undefined

traverseHsType (HsTyApp first second) =
    xmlNode "First"  (traverseHsType first)  [xsiType $ showHsType first] ++
    xmlNode "Second" (traverseHsType second) [xsiType $ showHsType second]

traverseHsType (HsTyVar name) = undefined
traverseHsType (HsTyCon qName) =
    xmlNode "Name" (traverseHsQName qName) [xsiType $ showHsQName qName]

traverseHsQOp :: HsQOp -> String
traverseHsQOp (HsQVarOp name) = 
    xmlNode "Name" (traverseHsQName name) [xsiType $ showHsQName name]
traverseHsQOp (HsQConOp name) = 
    xmlNode "Name" (traverseHsQName name) [xsiType $ showHsQName name]

traverseHsQName :: HsQName -> String
traverseHsQName (Qual mod name) = undefined
traverseHsQName (UnQual name)   = xmlNode "Name" (stripHsName name) noAttr
traverseHsQName (Special con)   =
    xmlNode "Value" noValue [xsiType $ showHsSpecialCon con]

traverseHsLiteral :: HsLiteral -> String
traverseHsLiteral (HsChar value)       = [value]
traverseHsLiteral (HsString value)     = value
traverseHsLiteral (HsInt value)        = show value
traverseHsLiteral (HsFrac value)       = show value
traverseHsLiteral (HsCharPrim value)   = [value]
traverseHsLiteral (HsStringPrim value) = value
traverseHsLiteral (HsIntPrim value)    = show value
traverseHsLiteral (HsFloatPrim value)  = show value
traverseHsLiteral (HsDoublePrim value) = show value

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

stripHsName :: HsName -> String
stripHsName (HsIdent  value) = value
stripHsName (HsSymbol value) = value

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile (head args)
  putStrLn $ (traverseModule . stripResult . parseModule) contents
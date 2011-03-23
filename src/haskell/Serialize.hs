module Serialize
    where

import Language.Haskell.Syntax

import Show
import XML

class Serializable a where
    serialize :: a -> String
    con       :: a -> String
    con _ = undefined

instance Serializable HsModule     where serialize = serializeHsModule
instance Serializable SrcLoc       where serialize = serializeSrcLoc
instance Serializable Module       where serialize = serializeMod
instance Serializable HsImportDecl where serialize = serializeHsImportDecl
instance Serializable HsImportSpec where serialize = serializeHsImportSpec
instance Serializable HsDecl       where serialize = serializeHsDecl
instance Serializable HsPat        where serialize = serializeHsPat
                                         con       = showHsPat
instance Serializable HsRhs        where serialize = serializeHsRhs
                                         con       = showHsRhs
instance Serializable HsExp        where serialize = serializeHsExp
                                         con       = showHsExp
instance Serializable HsQualType   where serialize = serializeHsQualType
instance Serializable HsType       where serialize = serializeHsType
                                         con       = showHsType
instance Serializable HsQOp        where serialize = serializeHsQOp
                                         con       = showHsQOp
instance Serializable HsQName      where serialize = serializeHsQName
                                         con       = showHsQName
instance Serializable HsLiteral    where serialize = serializeHsLiteral
                                         con       = showHsLiteral
instance Serializable HsName       where serialize = serializeHsName
instance Serializable HsSpecialCon where serialize = undefined
                                         con       = showHsSpecialCon

makeNode :: (Serializable a) => String -> a -> String
makeNode name value = xmlNode name (serialize value) [xsiType $ con value]

serializeHsModule :: HsModule -> String
serializeHsModule (HsModule srcLoc mod _ imports decl) =
    xmlNode "HsModule" 
                (
                 xmlNode "Location" (serialize srcLoc)      noAttr ++
                 xmlNode "Module"   (serialize mod)         noAttr ++
                 xmlNode "Imports"  (serialize =<< imports) noAttr ++
                 xmlNode "Body"     (serialize =<< decl)    noAttr
                ) 
                [
                 ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
                 ("xmlns:xsd", "http://www.w3.org/2001/XMLSchema")
                ]

serializeSrcLoc :: SrcLoc -> String
serializeSrcLoc (SrcLoc _ line column) =
    xmlNode "SrcLoc" noValue
                [
                 ("srcLine",   show line),
                 ("srcColumn", show column)
                ]

serializeMod :: Module -> String
serializeMod (Module mod) = mod

serializeHsImportDecl :: HsImportDecl -> String
serializeHsImportDecl (HsImportDecl srcLoc mod qual as specs) = 
    xmlNode "HsImportDecl" 
                (
                 xmlNode "Module"    (serialize mod) noAttr ++
                 xmlNode "Qualified" (show qual)     noAttr ++
                 stripAs as ++
                 stripSpecs specs
                ) noAttr 
        where 
          stripAs (Just as)              = 
              xmlNode "As" (serialize as) noAttr
          stripAs Nothing                = noValue
          stripSpecs (Just (bool, list)) = 
              xmlNode "SpecsExcluded" (show bool)          noAttr ++
              xmlNode "Specs"         (serialize =<< list) noAttr
          stripSpecs Nothing             = noValue

serializeHsImportSpec :: HsImportSpec -> String
serializeHsImportSpec (HsIVar name) = 
    xmlNode "HsIVar" (serialize name) noAttr
serializeHsImportSpec (HsIAbs name) =
    xmlNode "HsIAbs" (serialize name) noAttr
serializeHsImportSpec (HsIThingAll name) =
    xmlNode "HsIThingAll" (serialize name) noAttr
serializeHsImportSpec (HsIThingWith name _) =
    xmlNode "HsIThingWith" (serialize name) noAttr


serializeHsDecl :: HsDecl -> String
serializeHsDecl (HsTypeDecl srcLoc hsName hsNameList hsType) = 
    xmlNode "HsTypeDecl" (serialize hsName) noAttr

serializeHsDecl (HsDataDecl srcLoc hsContext hsName hsNameList hsConDeclList hsQNameList) =
    xmlNode "HsDataDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsInfixDecl srcLoc hsAssoc int hsOpList) =
    xmlNode "HsInfixDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNameList hsConDecl hsQNameList) =
    xmlNode "HsNewTypeDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsClassDecl srcLoc hsContext hsName hsNameList hsDeclList) =
    xmlNode "HsClassDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsInstDecl srcLoc hsContext hsQName hsTypeList hsDeclList) =
    xmlNode "HsInstDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsDefaultDecl srcLoc hsTypeList) =
    xmlNode "HsDefaultDecl" (serialize srcLoc) noAttr

serializeHsDecl (HsTypeSig srcLoc hsNameList hsQualType) =
    xmlNode "HsTypeSig" 
                (
                 serializeHsNameList hsNameList ++
                 serialize hsQualType
                ) noAttr

serializeHsDecl (HsFunBind hsMatchlist) =
    xmlNode "HsFunBind" noValue noAttr

serializeHsDecl (HsPatBind srcLoc pat rhs hsDeclList) =
    xmlNode "HsPatBind" 
                (
                 xmlNode "Pattern" (serialize pat) [xsiType $ con pat] ++
                 xmlNode "Rhs"     (serialize rhs) [xsiType $ con rhs]
                ) noAttr

-- currently unused
serializeHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = undefined
serializeHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = undefined

serializeHsPat :: HsPat -> String
serializeHsPat (HsPVar name)       = xmlNode "Name" (stripHsName name) noAttr
serializeHsPat (HsPLit _)          = "HsPLit"
serializeHsPat (HsPNeg _)          = "HsPNeg"
serializeHsPat (HsPInfixApp _ _ _) = "HsPInfixApp"
serializeHsPat (HsPApp _ _)        = "HsPApp"
serializeHsPat (HsPTuple _)        = "HsPTuple"
serializeHsPat (HsPList _)         = "HsPList"
serializeHsPat (HsPParen _)        = "HsPParen"
serializeHsPat (HsPRec _ _)        = "HsPRec"
serializeHsPat (HsPAsPat _ _)      = "HsPAsPat"
serializeHsPat (HsPWildCard)       = "HsPWildCard"
serializeHsPat (HsPIrrPat _)       = "HsPIrrPat"

serializeHsRhs :: HsRhs -> String
serializeHsRhs (HsUnGuardedRhs exp)  =
    makeNode "Expression" exp
serializeHsRhs (HsGuardedRhss _)   = "HsGuardedRhss"

serializeHsExp :: HsExp -> String
serializeHsExp (HsVar name) =
    makeNode "Name" name
serializeHsExp (HsCon name) =
    makeNode "Name" name
serializeHsExp (HsLit lit) =
    makeNode "Value" lit
serializeHsExp (HsInfixApp lhs op rhs) =
    makeNode "Left"  lhs ++
    makeNode "Op"    op  ++
    makeNode "Right" rhs
serializeHsExp (HsApp lhs rhs) =
    makeNode "Left"  lhs ++
    makeNode "Right" rhs

serializeHsExp (HsNegApp _)             = "HsNegApp"
serializeHsExp (HsLambda _ _ _)         = "HsLambda"
serializeHsExp (HsLet _ _)              = "HsLet"
serializeHsExp (HsIf _ _ _)             = "HsIf"
serializeHsExp (HsCase _ _)             = "HsCase"
serializeHsExp (HsDo _)                 = "HsDo"
serializeHsExp (HsTuple _)              = "HsTuple"
serializeHsExp (HsList _)               = "HsList"
serializeHsExp (HsParen _)              = "HsParen"
serializeHsExp (HsLeftSection _ _)      = "HsLeftSection"
serializeHsExp (HsRightSection op rhs) =
    makeNode "Op"    op ++
    makeNode "Right" rhs
serializeHsExp (HsRecConstr _ _)        = "HsRecConstr"
serializeHsExp (HsRecUpdate _ _)        = "HsRecUpdate"
serializeHsExp (HsEnumFrom _)           = "HsEnumFrom"
serializeHsExp (HsEnumFromTo from to) =
    makeNode "From" from ++
    makeNode "To"   to
serializeHsExp (HsEnumFromThen _ _)     = "HsEnumFromThen"
serializeHsExp (HsEnumFromThenTo _ _ _) = "HsEnumFromThenTo"
serializeHsExp (HsListComp _ _)         = "HsListComp"
serializeHsExp (HsExpTypeSig _ _ _)     = "HsExpTypeSig"
serializeHsExp (HsAsPat _ _)            = "HsAsPat"
serializeHsExp (HsWildCard)             = "HsWildCard"
serializeHsExp (HsIrrPat _)             = "HsIrrPat"

serializeHsNameList :: [HsName] -> String
serializeHsNameList hsNames =
    xmlNode "Names" (hsNames >>= serialize) noAttr

serializeHsQualType :: HsQualType -> String
serializeHsQualType (HsQualType context typ) =
    xmlNode "Type" 
                (
                 xmlNode "Context" (serializeHsAsstList context) noAttr ++
                 xmlNode "Type"    (serialize typ) [xsiType $ con typ]
                ) noAttr

serializeHsType :: HsType -> String
serializeHsType (HsTyFun inType outType) = undefined
serializeHsType (HsTyTuple typeList) = undefined

serializeHsType (HsTyApp first second) =
    makeNode "First"  first ++
    makeNode "Second" second

serializeHsType (HsTyVar name) = undefined
serializeHsType (HsTyCon qName) =
    makeNode "Name" qName

serializeHsQOp :: HsQOp -> String
serializeHsQOp (HsQVarOp name) = 
    makeNode "Name" name
serializeHsQOp (HsQConOp name) = 
    makeNode "Name" name

serializeHsQName :: HsQName -> String
serializeHsQName (Qual mod name) = undefined
serializeHsQName (UnQual name)   = xmlNode "Name" (stripHsName name) noAttr
serializeHsQName (Special constr) =
    xmlNode "Value" noValue [xsiType $ con constr]

serializeHsLiteral :: HsLiteral -> String
serializeHsLiteral (HsChar value)       = [value]
serializeHsLiteral (HsString value)     = value
serializeHsLiteral (HsInt value)        = show value
serializeHsLiteral (HsFrac value)       = show value
serializeHsLiteral (HsCharPrim value)   = [value]
serializeHsLiteral (HsStringPrim value) = value
serializeHsLiteral (HsIntPrim value)    = show value
serializeHsLiteral (HsFloatPrim value)  = show value
serializeHsLiteral (HsDoublePrim value) = show value

-- todo
serializeHsAsst :: HsAsst -> String
serializeHsAsst asst = undefined

serializeHsAsstList :: [HsAsst] -> String
serializeHsAsstList assts =
    xmlNode "Assertions" (assts >>= serializeHsAsst) noAttr

-- we currently do not differ these two
serializeHsName :: HsName -> String
serializeHsName (HsIdent  value) = xmlNode "HsName" value noAttr
serializeHsName (HsSymbol value) = xmlNode "HsName" value noAttr

stripHsName :: HsName -> String
stripHsName (HsIdent  value) = value
stripHsName (HsSymbol value) = value

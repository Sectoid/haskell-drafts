module Show
    where

import Language.Haskell.Syntax

showHsType :: HsType -> String
showHsType (HsTyFun _ _)           = "HsTyFun"
showHsType (HsTyTuple _)           = "HsTyTuple"
showHsType (HsTyApp _ _)           = "HsTyApp"
showHsType (HsTyVar _)             = "HsTyVar"
showHsType (HsTyCon _)             = "HsTyCon"

showHsQName :: HsQName -> String
showHsQName (Qual _ _)             = "Qual"
showHsQName (UnQual _)             = "UnQual"
showHsQName (Special _)            = "Special"

showHsSpecialCon :: HsSpecialCon -> String
showHsSpecialCon HsUnitCon         = "HsUnitCon"
showHsSpecialCon HsListCon         = "HsListCon"
showHsSpecialCon HsFunCon          = "HsFunCon"
showHsSpecialCon (HsTupleCon _)    = "HsTupleCon"
showHsSpecialCon HsCons            = "HsCons"

showHsPat :: HsPat -> String
showHsPat (HsPVar _)               = "HsPVar"
showHsPat (HsPLit _)               = "HsPLit"
showHsPat (HsPNeg _)               = "HsPNeg"
showHsPat (HsPInfixApp _ _ _)      = "HsPInfixApp"
showHsPat (HsPApp _ _)             = "HsPApp"
showHsPat (HsPTuple _)             = "HsPTuple"
showHsPat (HsPList _)              = "HsPList"
showHsPat (HsPParen _)             = "HsPParen"
showHsPat (HsPRec _ _)             = "HsPRec"
showHsPat (HsPAsPat _ _)           = "HsPAsPat"
showHsPat (HsPWildCard)            = "HsPWildCard"
showHsPat (HsPIrrPat _)            = "HsPIrrPat"

showHsRhs :: HsRhs -> String
showHsRhs (HsUnGuardedRhs _)       = "HsUnGuardedRhs"
showHsRhs (HsGuardedRhss _)        = "HsGuardedRhss"

showHsExp :: HsExp -> String
showHsExp (HsVar _)                = "HsVar"
showHsExp (HsCon _)                = "HsCon"
showHsExp (HsLit _)                = "HsLit"
showHsExp (HsInfixApp _ _ _)       = "HsInfixApp"
showHsExp (HsApp _ _)              = "HsApp"
showHsExp (HsNegApp _)             = "HsNegApp"
showHsExp (HsLambda _ _ _)         = "HsLambda"
showHsExp (HsLet _ _)              = "HsLet"
showHsExp (HsIf _ _ _)             = "HsIf"
showHsExp (HsCase _ _)             = "HsCase"
showHsExp (HsDo _)                 = "HsDo"
showHsExp (HsTuple _)              = "HsTuple"
showHsExp (HsList _)               = "HsList"
showHsExp (HsParen _)              = "HsParen"
showHsExp (HsLeftSection _ _)      = "HsLeftSection"
showHsExp (HsRightSection _ _)     = "HsRightSection"
showHsExp (HsRecConstr _ _)        = "HsRecConstr"
showHsExp (HsRecUpdate _ _)        = "HsRecUpdate"
showHsExp (HsEnumFrom _)           = "HsEnumFrom"
showHsExp (HsEnumFromTo _ _)       = "HsEnumFromTo"
showHsExp (HsEnumFromThen _ _)     = "HsEnumFromThen"
showHsExp (HsEnumFromThenTo _ _ _) = "HsEnumFromThenTo"
showHsExp (HsListComp _ _)         = "HsListComp"
showHsExp (HsExpTypeSig _ _ _)     = "HsExpTypeSig"
showHsExp (HsAsPat _ _)            = "HsAsPat"
showHsExp (HsWildCard)             = "HsWildCard"
showHsExp (HsIrrPat _)             = "HsIrrPat"

showHsQOp :: HsQOp -> String
showHsQOp (HsQVarOp _)             = "HsQVarOp"	
showHsQOp (HsQConOp _)	           = "HsQConOp"

showHsLiteral :: HsLiteral -> String
showHsLiteral (HsChar _)           = "HsChar"
showHsLiteral (HsString _)         = "HsString"
showHsLiteral (HsInt _)            = "HsInt"
showHsLiteral (HsFrac _)           = "HsFrac"
showHsLiteral (HsCharPrim _)       = "HsCharPrim"
showHsLiteral (HsStringPrim _)     = "HsStringPrim"
showHsLiteral (HsIntPrim _)        = "HsIntPrim"
showHsLiteral (HsFloatPrim _)      = "HsFloatPrim"
showHsLiteral (HsDoublePrim _)     = "HsDoublePrim"

module Show
    where

import Language.Haskell.Syntax

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

showHsPat :: HsPat -> String
showHsPat (HsPVar _)            = "HsPVar"
showHsPat (HsPLit _)            = "HsPLit"
showHsPat (HsPNeg _)            = "HsPNeg"
showHsPat (HsPInfixApp _ _ _)   = "HsPInfixApp"
showHsPat (HsPApp _ _)          = "HsPApp"
showHsPat (HsPTuple _)          = "HsPTuple"
showHsPat (HsPList _)           = "HsPList"
showHsPat (HsPParen _)          = "HsPParen"
showHsPat (HsPRec _ _)          = "HsPRec"
showHsPat (HsPAsPat _ _)        = "HsPAsPat"
showHsPat (HsPWildCard)         = "HsPWildCard"
showHsPat (HsPIrrPat _)         = "HsPIrrPat"
// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public interface IVisitor
{
  void visit(SrcLoc node);
  void visit(HsName node);
  void visit(UnQual node);
  void visit(HsModule node);
  void visit(HsTypeSig node);
  void visit(HsQualType node);
  void visit(HsContext node);
  void visit(HsPatBind node);
  void visit(HsTyApp node);
  void visit(HsTyCon node);
  void visit(HsPVar node);
  void visit(HsUnGuardedRhs node);
  void visit(HsInfixApp node);
  void visit(HsApp node);
  void visit(HsVar node);
  void visit(HsRightSection node);
  void visit(HsLit node);
  void visit(HsEnumFromTo node);
  void visit(HsInt node);
  void visit(Qual node);
  void visit(Special node);
  void visit(HsUnitCon node);
  void visit(HsListCon node);
  void visit(HsFunCon node);
  void visit(HsTupleCon node);
  void visit(HsCons node);
  void visit(HsQVarOp node);
  void visit(HsQConOp node);
  void visit(HsAsst node);
  void visit(Module node);
  void visit(HsImportDecl node);
  void visit(HsIVar node);
  void visit(HsIAbs node);
  void visit(HsIThingAll node);
  void visit(HsIThingWith node);
  void visit(HsVarName node);
  void visit(HsConName node);
}

}

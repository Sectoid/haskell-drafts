// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public abstract class EmptyVisitor : IVisitor
{
  public virtual void visit(SrcLoc node) {}
  public virtual void visit(HsName node) {}
  public virtual void visit(UnQual node) {}
  public virtual void visit(HsModule node) {}
  public virtual void visit(HsQualType node) {}
  public virtual void visit(HsTypeSig node) {}
  public virtual void visit(HsContext node) {}
  public virtual void visit(HsPatBind node) {}
  public virtual void visit(HsTyApp node) {}
  public virtual void visit(HsTyCon node) {}
  public virtual void visit(HsPVar node) {}
  public virtual void visit(HsUnGuardedRhs node) {}
  public virtual void visit(HsInfixApp node) {}
  public virtual void visit(HsApp node) {}
  public virtual void visit(HsVar node) {}
  public virtual void visit(HsRightSection node) {}
  public virtual void visit(HsLit node) {}
  public virtual void visit(HsEnumFromTo node) {}
  public virtual void visit(HsInt node) {}
  public virtual void visit(Qual node) {}
  public virtual void visit(Special node) {}
  public virtual void visit(HsUnitCon node) {}
  public virtual void visit(HsListCon node) {}
  public virtual void visit(HsFunCon node) {}
  public virtual void visit(HsTupleCon node) {}
  public virtual void visit(HsCons node) {}
  public virtual void visit(HsQVarOp node) {}
  public virtual void visit(HsQConOp node) {}
  public virtual void visit(HsAsst node) {}
  public virtual void visit(Module node) {}
  public virtual void visit(HsImportDecl node) {}
  public virtual void visit(HsIVar node) {}
  public virtual void visit(HsIAbs node) {}
  public virtual void visit(HsIThingAll node) {}
  public virtual void visit(HsIThingWith node) {}
  public virtual void visit(HsVarName node) {}
  public virtual void visit(HsConName node) {}
}

public static class VisitingExtensions
{
  public static void walkDFS(this IAstNode node, Action<IAstNode> fn)
  {
    // Console.WriteLine("Entering node: {0}", node.ToString());
    fn(node);
    foreach(var child in node.Children)
      child.walkDFS(fn);
  }

  public static void walkDFSEnd(this IAstNode node, Action<IAstNode> fn)
  {
    // Console.WriteLine("Entering node: {0}", node.ToString());
    foreach(var child in node.Children)
      child.walkDFSEnd(fn);
    fn(node);
  }

  public static void visitDFS(this IAstNode node, IVisitor visitor)
  {
    node.walkDFS(x => x.accept(visitor));
  }

  public static void visitDFSEnd(this IAstNode node, IVisitor visitor)
  {
    node.walkDFSEnd(x => x.accept(visitor));
  }
}

}

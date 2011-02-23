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
}

public class DFSVisitor : IVisitor
{
  public IVisitor Strategy { get; private set; }
  public DFSVisitor(IVisitor strategy) { Strategy = strategy; }

  public virtual void visit(HsModule node) 
  { 
    node.Location.accept(this);
    foreach(var bodyNode in node.Body)
      bodyNode.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(SrcLoc node) { node.accept(Strategy); }
  public virtual void visit(HsName node) { node.accept(Strategy); }
  public virtual void visit(UnQual node) { node.accept(Strategy); }
  public virtual void visit(HsQualType node) 
  {
    node.Context.accept(this);
    node.Type.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsTypeSig node) 
  {
    foreach(var nameNode in node.Names)
      nameNode.accept(this);
    node.Type.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsContext node) { node.accept(Strategy); }
  public virtual void visit(HsPatBind node) 
  {
    node.Pattern.accept(this);
    node.Rhs.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsTyApp node) 
  {
    node.First.accept(this);
    node.Second.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsTyCon node) 
  {
    node.Name.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsPVar node) 
  {
    node.Name.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsUnGuardedRhs node) 
  {
    node.Expression.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsInfixApp node) 
  {
    node.Left.accept(this);
    node.Op.accept(this);
    node.Right.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsApp node) 
  {
    node.Left.accept(this);
    node.Right.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsVar node) 
  {
    node.Name.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsRightSection node) 
  {
    node.Op.accept(this);
    node.Right.accept(this);
    node.accept(Strategy);    
  }

  public virtual void visit(HsLit node) 
  {
    node.Value.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsEnumFromTo node) 
  {
    node.From.accept(this);
    node.To.accept(this);
    node.accept(Strategy);    
  }

  public virtual void visit(HsInt node) 
  {
    node.accept(Strategy);
  }

  public virtual void visit(Qual node)
  {
    node.accept(Strategy);
  }

  public virtual void visit(Special node)
  {
    node.Value.accept(this);
    node.accept(Strategy);
  }

  public virtual void visit(HsUnitCon node) { node.accept(Strategy); }
  public virtual void visit(HsListCon node) { node.accept(Strategy); }
  public virtual void visit(HsFunCon node) { node.accept(Strategy); }
  public virtual void visit(HsTupleCon node) { node.accept(Strategy); }
  public virtual void visit(HsCons node) { node.accept(Strategy); }

}

}

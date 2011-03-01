// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Xml.Serialization;

using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public class EchoVisitor : IVisitor
{
  public virtual void visit(SrcLoc node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsName node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(UnQual node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsModule node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsQualType node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsTypeSig node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsContext node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsPatBind node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsTyApp node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsTyCon node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsPVar node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsUnGuardedRhs node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsInfixApp node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsApp node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsVar node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsRightSection node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsLit node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsEnumFromTo node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsInt node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }

  public virtual void visit(Qual node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(Special node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsUnitCon node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsListCon node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsFunCon node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsTupleCon node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsCons node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsQVarOp node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsQConOp node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsAsst node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(Module node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsImportDecl node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsIVar node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsIAbs node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsIThingAll node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsIThingWith node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsVarName node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
  public virtual void visit(HsConName node)
  {
    Console.WriteLine("Visiting {0}", node.ToString());
  }
}

}
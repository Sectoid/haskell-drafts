// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Xml.Serialization;


namespace Language.Haskell.Phase1.DOM
{

public interface IAstNode
{
  IAstNode Parent { get; set; }
  void accept(IVisitor visitor);
}

public abstract class BasicNode : IAstNode
{
  [XmlIgnore]
  public IAstNode Parent { get; set; }
  public abstract void accept(IVisitor visitor);
}

public class SrcLoc : BasicNode
{
  [XmlAttribute("srcLine")]
  public int Line { get; set; }
  [XmlAttribute("srcColumn")]
  public int Column { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsName : BasicNode
{
  [XmlText]
  public string Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsQName : BasicNode {}

public class UnQual : HsQName
{
  [XmlText]
  public string Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class Qual : HsQName
{
  public string Module { get; set; }
  public string Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class Special : HsQName
{
  public HsSpecialCon Value { get; set; }
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsSpecialCon : BasicNode {}

public class HsUnitCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsListCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsFunCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsTupleCon : HsSpecialCon
{
  public int Count { get; set; }
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsCons : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsModule : BasicNode
{
  public SrcLoc Location { get; set; }
  public string Module { get; set; }

  [XmlArray ("Body")]
  [XmlArrayItem("HsTypeSig", typeof(HsTypeSig))]
  [XmlArrayItem("HsPatBind", typeof(HsPatBind))]
  public List<HsDecl> Body { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsDecl : BasicNode
{
  public SrcLoc Location { get; set; }
}

public class HsTypeSig : HsDecl
{
  [XmlArray ("Names")]
  [XmlArrayItem("HsName", typeof(HsName))]
  public List<HsName> Names { get; set; }
  public HsQualType Type { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsQualType : BasicNode
{
  public HsContext Context { get; set; }
  public HsType Type { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsContext : BasicNode 
{
  [XmlArrayItem("HsAsst", typeof(HsAsst))]
  public List<HsAsst> Assertions { get; set; }
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsAsst : BasicNode
{
  public HsQName Name { get; set; }

  [XmlArray("Types")]
  [XmlArrayItem("HsType", typeof(HsType))]
  public List<HsType> Types { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsType : BasicNode {}

public class HsTyApp : HsType
{
  public HsType First { get; set; }
  public HsType Second { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsTyCon : HsType
{
  public HsQName Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsPatBind : HsDecl
{
  public HsPat Pattern { get; set; }
  public HsRhs Rhs { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsPat : BasicNode {}

public class HsPVar : HsPat
{
  public HsName Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsRhs : BasicNode {}

public class HsUnGuardedRhs : HsRhs
{
  public HsExp Expression { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsExp : BasicNode {}

public class HsInfixApp : HsExp
{
  public HsExp Left { get; set; }
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsApp : HsExp
{
  public HsExp Left { get; set; }
  public HsExp Right { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsVar : HsExp
{
  public HsQName Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsRightSection : HsExp
{
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsLit : HsExp
{
  public HsLiteral Value { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsEnumFromTo : HsExp
{
  public HsExp From { get; set; }
  public HsExp To { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsLiteral : BasicNode {}

public class HsInt : HsLiteral
{
  [XmlText]
  public string Value;

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsQOp : BasicNode {}

public class HsQVarOp : HsQOp
{
  public HsQName Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public class HsQConOp : HsQOp
{
  public HsQName Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}


}
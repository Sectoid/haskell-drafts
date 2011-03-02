 // -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Language.Haskell.Phase1.DOM
{

public interface IAstNode
{
  IAstNode Parent { get; set; }
  IEnumerable<IAstNode> Children { get; }

  void accept(IVisitor visitor);
}

public abstract class BasicNode : IAstNode
{
  [XmlIgnore]
  public IAstNode Parent { get; set; }

  [XmlIgnore]
  public virtual IEnumerable<IAstNode> Children { get { yield break; } }

  public abstract void accept(IVisitor visitor);
}

public partial class SrcLoc : BasicNode
{
  // [XmlAttribute("fileName")]
  [XmlIgnore]
  public string FileName { get; set; }
  [XmlAttribute("srcLine")]
  public int Line { get; set; }
  [XmlAttribute("srcColumn")]
  public int Column { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsName : BasicNode
{
  [XmlText]
  public string Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

  public override string ToString()
  {
    return Name;
  }
}

public abstract class HsQName : BasicNode {}

public partial class UnQual : HsQName
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    { 
      yield return Name;
    }
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class Qual : HsQName
{
  public Module Module { get; set; }
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children
  {
    get
    {
      yield return Module;
      yield return Name;
    }
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class Special : HsQName
{
  public HsSpecialCon Value { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Value; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class Module : BasicNode
{
  public string Name { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsSpecialCon : BasicNode {}

public partial class HsUnitCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsListCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsFunCon : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsTupleCon : HsSpecialCon
{
  public int Count { get; set; }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsCons : HsSpecialCon
{
  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsModule : BasicNode
{
  public SrcLoc Location { get; set; }
  public Module Module { get; set; }

  [XmlArray("Imports")]
  [XmlArrayItem("HsImportDecl", typeof(HsImportDecl))]
  public List<HsImportDecl> Imports { get; set; }

  [XmlArray ("Body")]
  [XmlArrayItem("HsTypeSig", typeof(HsTypeSig))]
  [XmlArrayItem("HsPatBind", typeof(HsPatBind))]
  public List<HsDecl> Body { get; set; }


  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      if(Location != null)
        yield return Location;
      yield return Module;
      foreach (var @import in Imports)
        yield return @import;
      foreach (var child in Body)
        yield return child;
    }
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsDecl : BasicNode
{
  public SrcLoc Location { get; set; }
}

public partial class HsTypeSig : HsDecl
{
  [XmlArray ("Names")]
  [XmlArrayItem("HsName", typeof(HsName))]
  public List<HsName> Names { get; set; }
  public HsQualType Type { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      if(Location != null)
        yield return Location;
      foreach(var child in Names)
        yield return child;
      yield return Type;
    }
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsQualType : BasicNode
{
  public HsContext Context { get; set; }
  public HsType Type { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    { 
      yield return Context;
      yield return Type; 
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsContext : BasicNode 
{
  [XmlArrayItem("HsAsst", typeof(HsAsst))]
  public List<HsAsst> Assertions { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      foreach(var child in Assertions)
        yield return child;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsAsst : BasicNode
{
  public HsQName Name { get; set; }

  [XmlArray("Types")]
  [XmlArrayItem("HsType", typeof(HsType))]
  public List<HsType> Types { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
      foreach(var child in Types)
        yield return child;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsType : BasicNode {}

public partial class HsTyApp : HsType
{
  public HsType First { get; set; }
  public HsType Second { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return First;
      yield return Second;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsTyCon : HsType
{
  public HsQName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsPatBind : HsDecl
{
  public HsPat Pattern { get; set; }
  public HsRhs Rhs { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Pattern;
      yield return Rhs;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsPat : BasicNode {}

public partial class HsPVar : HsPat
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsRhs : BasicNode {}

public partial class HsUnGuardedRhs : HsRhs
{
  public HsExp Expression { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Expression;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsExp : BasicNode {}

public partial class HsInfixApp : HsExp
{
  public HsExp Left { get; set; }
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Left;
      yield return Op;
      yield return Right;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsApp : HsExp
{
  public HsExp Left { get; set; }
  public HsExp Right { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Left;
      yield return Right;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsVar : HsExp
{
  public HsQName Name { get; set; }
  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsRightSection : HsExp
{
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Op;
      yield return Right;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsLit : HsExp
{
  public HsLiteral Value { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Value;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsEnumFromTo : HsExp
{
  public HsExp From { get; set; }
  public HsExp To { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return From;
      yield return To;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsLiteral : BasicNode {}

public partial class HsInt : HsLiteral
{
  [XmlText]
  public string Value;

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsQOp : BasicNode {}

public partial class HsQVarOp : HsQOp
{
  public HsQName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsQConOp : HsQOp
{
  public HsQName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Name;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public partial class HsImportDecl : BasicNode
{
  public SrcLoc Location { get; set; }
  public Module Module { get; set; }
  public bool Qualified { get; set; }
  public Module As { get; set; } // This is an option
  public bool SpecsExcluded { get; set; }

  [XmlArray ("Specs")]
  [XmlArrayItem("HsIVar", typeof(HsIVar))]
  [XmlArrayItem("HsIAbs", typeof(HsIAbs))]
  [XmlArrayItem("HsIThingAll", typeof(HsIThingAll))]
  [XmlArrayItem("HsIThingWith", typeof(HsIThingWith))]
  public List<HsImportSpec> Specs { get; set; } // This is an option

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    {
      yield return Location;
      yield return Module;
      if(As != null) yield return As;
      if(Specs != null)
        foreach(var spec in Specs)
          yield return spec;
    }
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }
}

public abstract class HsImportSpec : BasicNode {}

public partial class HsIVar : HsImportSpec
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Name; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}

public partial class HsIAbs : HsImportSpec
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Name; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}

public partial class HsIThingAll : HsImportSpec
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Name; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}

public partial class HsIThingWith : HsImportSpec
{
  public HsName Name { get; set; }

  [XmlArray ("Components")]
  [XmlArrayItem("HsVarName", typeof(HsVarName))]
  [XmlArrayItem("HsConName", typeof(HsConName))]
  public List<HsCName> Components { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children 
  { 
    get 
    { 
      yield return Name;
      foreach(var comp in Components)
        yield return comp;
    } 
  }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}

public abstract class HsCName : BasicNode {}

public partial class HsVarName : HsCName
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Name; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}

public partial class HsConName : HsCName
{
  public HsName Name { get; set; }

  [XmlIgnore]
  public override IEnumerable<IAstNode> Children { get { yield return Name; } }

  public override void accept(IVisitor visitor)
  {
    visitor.visit(this);
  }

}


}
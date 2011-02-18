// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Xml.Serialization;


namespace Language.Haskell.Phase1.DOM
{

public class SrcLoc
{
  [XmlAttribute("srcLine")]
  public int Line { get; set; }
  [XmlAttribute("srcColumn")]
  public int Column {get; set; }
}

public class HsName
{
  [XmlText]
  public string Name { get; set; }
}

public abstract class HsQName {}

public class UnQual : HsQName
{
  [XmlText]
  public string Name { get; set; }
}

public class HsModule
{
  public SrcLoc Location { get; set; }
  public string Module { get; set; }
  public List<HsDecl> Body { get; set; }
}

public abstract class HsDecl 
{
  public SrcLoc Location { get; set; }
}

public class HsTypeSig : HsDecl
{
  public List<HsName> Names { get; set; }
  public HsQualType Type { get; set; }
}

public class HsQualType
{
  public HsContext Context { get; set; }
  public HsType Type { get; set; }
}

public class HsContext {}

public abstract class HsType {}

public class HsTyApp : HsType
{
  public HsType First { get; set; }
  public HsType Second { get; set; }
}

public class HsTyCon
{
  public HsQName Name { get; set; }
}

public class HsPatBind : HsDecl
{
  public HsPat Pattern { get; set; }
  public HsRhs Rhs { get; set; }
}

public abstract class HsPat {}

public class HsPVar : HsPat
{
  public HsName Name { get; set; }
}

public abstract class HsRhs {}

public class HsUnGuardedRhs : HsRhs
{
  public HsExp Expression { get; set; }
}

public abstract class HsExp {}

public class HsInfixApp : HsExp
{
  public HsExp Left { get; set; }
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }
}

public class HsApp : HsExp
{
  public HsExp Left { get; set; }
  public HsExp Right { get; set; }
}

public class HsVar : HsExp
{
  public HsQName Name { get; set; }
}

public class HsRightSection : HsExp
{
  public HsQOp Op { get; set; }
  public HsExp Right { get; set; }
}

public class HsLit : HsExp
{
  public HsLiteral Value { get; set; }
}

public class HsEnumFromTo : HsExp
{
  public HsExp From { get; set; }
  public HsExp To { get; set; }
}

public abstract class HsLiteral {}

public abstract class HsInt : HsLiteral
{
  [XmlText]
  public int Value;
}

public abstract class HsQOp {}

}
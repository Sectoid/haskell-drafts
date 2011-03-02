 // -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Xml.Serialization;

namespace Language.Haskell.Phase1.DOM
{

public partial class HsModule
{
  [XmlIgnore]
  public Type DeclaringType { get; set; }
}

public partial class HsImportDecl
{
  [XmlIgnore]
  public Type ModuleDeclaringType { get; set; }
}

}

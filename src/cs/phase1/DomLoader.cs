// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Collections.Generic;
using System.Xml.Serialization;

using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public class DOMLoader
{
  private XmlSerializer serializer = null;

  public DOMLoader()
  {
    var assembly = typeof(BasicNode).Assembly;
    var allTypes = assembly.GetTypes();
    var effectiveTypes = new List<Type>();
    foreach(var type in allTypes)
      if(type.IsSubclassOf(typeof(BasicNode)))
        effectiveTypes.Add(type);

    serializer = new XmlSerializer(typeof(HsModule), effectiveTypes.ToArray());
  }

  public HsModule load(Stream input)
  {
    return fillParent(serializer.Deserialize(input) as HsModule, null);
  }

  public HsModule load(TextReader input)
  {
    return fillParent(serializer.Deserialize(input) as HsModule, null);
  }

  public void save(Stream output, HsModule module)
  {
    serializer.Serialize(output, module);
  }

  public void save(TextWriter output, HsModule module)
  {
    serializer.Serialize(output, module);
  }

  private Node fillParent<Node>(Node node, Node parent)
    where Node : IAstNode
  {
    node.Parent = parent;
    foreach(var child in node.Children)
      fillParent(child, node);

    return node;
  }
}

}

// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Xml.Serialization;

using NUnit.Framework;
using NUnit.Framework.SyntaxHelpers;

namespace Language.Haskell.Phase1.DOM
{

[TestFixture]
public class DomTests
{

  [Test]
  public void SillyDeserialization()
  {
    using(var reader = new StringReader(TestData.XmlAST))
    {
      var loader = new XmlSerializer(typeof(HsModule));
      var module = loader.Deserialize(reader);

      Assert.That(module, Is.Not.Null);
      Assert.That(module, Is.TypeOf(typeof(HsModule)));
    }
  }

}

}
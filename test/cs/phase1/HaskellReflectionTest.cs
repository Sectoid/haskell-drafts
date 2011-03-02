// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Xml.Serialization;

using NUnit.Framework;
using NUnit.Framework.SyntaxHelpers;

namespace Language.Haskell.Phase1
{

[TestFixture]
public class HaskellReflectionTest
{
  [Test]
  public void SillyDeserialization()
  {
    var preludeAssembly = typeof(Prelude).Assembly;

    Assert.That(preludeAssembly, Is.Not.Null);
    Assert.That(preludeAssembly.findModule("Prelude"), Is.Not.Null);
  }
  
}

}
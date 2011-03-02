// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Xml.Serialization;

using NUnit.Framework;
using NUnit.Framework.SyntaxHelpers;

namespace Language.Haskell.Phase1
{

[TestFixture]
public class QualifierTests
{

  private Stream dataStream1 = null;

  [SetUp]
  public void Init()
  {
    var assembly = this.GetType().Assembly;
    dataStream1 = assembly.GetManifestResourceStream("Language.Haskell.Phase1.Tests.Data.Test1.xml");
  }

  [Test]
  public void Debug()
  {
    var loader = new DOMLoader();
    var module = loader.load(dataStream1);
    module.visitDFS(new Qualifier());
  }

}

}
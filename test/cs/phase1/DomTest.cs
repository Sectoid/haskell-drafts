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

  private Stream dataStream1 = null;

  [SetUp]
  public void Init()
  {
    var assembly = this.GetType().Assembly;
    dataStream1 = assembly.GetManifestResourceStream("Language.Haskell.Phase1.Tests.Data.Test1.xml");
  }

  [Test]
  public void SillyDeserialization()
  {
    var loader = new DOMLoader();
    var module = loader.load(dataStream1);
    
    Assert.That(module, Is.Not.Null);
  }

  [Test]
  public void TestVisitDFS()
  {
    var loader = new DOMLoader();
    var module = loader.load(dataStream1);
    
    module.visitDFSEnd(new EchoVisitor());
  }

  [Test]
  public void TestParentFill()
  {
    var loader = new DOMLoader();
    var module = loader.load(dataStream1);
    
    module.walkDFS(x =>
      {
        if(x != module)
          Assert.That(x.Parent, Is.Not.Null);
      });
  }

  [Test]
  public void TestSerialize()
  {
    using(var writer = new StringWriter())
    {
      var astObj = new HsModule
        { Location = new SrcLoc { Line = 1, Column = 1 },
          Module = new Module { Name = "Crash" },
          Body = new System.Collections.Generic.List<HsDecl>(new HsDecl [] {
              new HsTypeSig {
                Names = new System.Collections.Generic.List<HsName>( new [] {
                    new HsName { Name = "main", }
                  } ),
                Type = new HsQualType {
                  Context = new HsContext {
                    Assertions = new System.Collections.Generic.List<HsAsst>(new [] {
                        new HsAsst {
                          Name = new UnQual { Name = new HsName { Name = "Nya", }, },
                        }
                      }),
                  },
                  Type = new HsTyApp {
                    First = new HsTyCon {
                      Name = new UnQual { Name = new HsName { Name = "IO", }, },
                    },
                    Second = new HsTyCon {
                      Name = new Special { Value = new HsUnitCon {}, },
                    },
                  }
                },
              },
            }), };
      var dumper = new DOMLoader();
      dumper.save(writer, astObj);
      Console.WriteLine("Output: {0}", writer.ToString());
    }
  }
}

}
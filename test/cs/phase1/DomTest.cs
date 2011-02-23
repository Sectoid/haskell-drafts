// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.IO;
using System.Xml.Serialization;

using NUnit.Framework;
using NUnit.Framework.SyntaxHelpers;

namespace Language.Haskell.Phase1.DOM
{

public class DebugVisitor : IVisitor
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
}


[TestFixture]
public class DomTests
{

  [Test]
  public void SillyDeserialization()
  {
    using(var reader = new StringReader(TestData.XmlAST))
    {
      var loader = new DOMLoader();
      var module = loader.load(reader);

      Assert.That(module, Is.Not.Null);
    }
  }

  [Test]
  public void TestVisitDFS()
  {
    using(var reader = new StringReader(TestData.XmlAST))
    {
      var loader = new DOMLoader();
      var module = loader.load(reader);

      module.visitDFSEnd(new DebugVisitor());
    }
  }

  [Test]
  public void TestParentFill()
  {
    using(var reader = new StringReader(TestData.XmlAST))
    {
      var loader = new DOMLoader();
      var module = loader.load(reader);

      module.walkDFS(x =>
        {
          if(x != module)
            Assert.That(x.Parent, Is.Not.Null);
        });
    }
  }

  [Test]
  public void TestSerialize()
  {
    using(var writer = new StringWriter())
    {
      // var astObj = new HsPatBind 
      //   { Pattern = new HsPVar 
      //     { Name = new HsName 
      //       { Name = "main" } } };
      var astObj = new HsModule
        { Location = new SrcLoc { Line = 1, Column = 1 },
          Module = "Crash",
          Body = new System.Collections.Generic.List<HsDecl>(new HsDecl [] {
              new HsTypeSig {
                Names = new System.Collections.Generic.List<HsName>( new [] {
                    new HsName { Name = "main", }
                  } ),
                Type = new HsQualType {
                  Context = new HsContext {
                    Assertions = new System.Collections.Generic.List<HsAsst>(new [] {
                        new HsAsst {
                          Name = new UnQual { Name = "Nya", }
                        }
                      }),
                  },
                  Type = new HsTyApp {
                    First = new HsTyCon {
                      Name = new UnQual { Name = "IO" },
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
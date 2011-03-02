// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;

using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public static class QualifierExtensions
{
  public static Type findModule(this Assembly asm, string name)
  {
    // This code somewhy fails NUnit
    // return asm.GetTypes()
    //   .Where(x => x.IsSubclassOf(typeof(Runtime.Module)) && x.Name == name)
    //   .FirstOrDefault();

    var types = asm.GetTypes();

    foreach(var type in types)
      if(type.IsSubclassOf(typeof(Runtime.Module)) && (type.Name == name))
        return type;
    
    return null;
  }
}


public class Qualifier : EmptyVisitor
{
  List<Assembly> placesToLook = new List<Assembly>();

  // public override void visit(HsModule node)
  // {
    // Console.WriteLine("Processing HsModule node");
    // Console.WriteLine("Imports: {0}", node.Imports);
    // foreach(var importDecl in node.Imports)
    // {
      
    //   Console.WriteLine("Import: {0}", importDecl);
    // }
  // }

  public override void visit(HsTyCon node)
  {
    // if(node.Name is UnQual)
    Console.WriteLine("Found type constructor. Name => {0}", node.Name);
    //ResolveTypeConstructor()
  }

  // public override void visit(HsImportDecl node)
  // {
  //   var moduleName = node.Module;
  //   foreach(var asm in placesToLook)
  //   {
  //     var module = asm.FindModule(moduleName);
  //     if(module != null) currentModule = module;
      
  //     // node.walkChilrenDPS(this);
  //   }
  // }
  // public override void visit(UnQual node)
  // {
  //   Console.WriteLine("Found unqualified node. Name => {0}", node.Name.Name);
  // }
}

}

// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Linq;

using Language.Haskell.Phase1.DOM;

namespace Language.Haskell.Phase1
{

public class UnableToFindModuleException : LocatedErrorException
{
  public UnableToFindModuleException(SrcLoc location, string moduleName)
  : base(location, 
         String.Format("Unable to find assembly containing module '{0}'. Maybe you missed an assembly reference?", moduleName.ToString()))
  {}
  
}

public static class QualifierExtensions
{
  public static Type findModule(this Assembly asm, string name)
  {
    // This code somewhy fails NUnit
    return asm.GetTypes()
      .Where(x => x.IsSubclassOf(typeof(Runtime.Module)) && x.Name == name)
      .FirstOrDefault();

    // var types = asm.GetTypes();

    // foreach(var type in types)
    //   if(type.IsSubclassOf(typeof(Runtime.Module)) && (type.Name == name))
    //     return type;
    
    // return null;
  }

  public static IEnumerable<Type> allExports(this Type module)
  {
    // TODO: Assert module is a Module
    var retVal = module
      .GetCustomAttributes(typeof(Runtime.ExportAttribute), false)
      .OfType<Runtime.ExportAttribute>()
      .Select(x => x.Type);

    Console.WriteLine("Found {0} exports", retVal.Count());

    return retVal;
    // return module
    //   .GetCustomAttributes(typeof(Runtime.ExportAttribute), false)
    //   .OfType<Runtime.ExportAttribute>()
    //   .Select(x => x.Type);
  }

  public static IEnumerable<Type> typeClasses(this IEnumerable<Type> exports)
  {
    return exports.Where(x => x.GetCustomAttributes(typeof(Runtime.TypeClassAttribute), 
                                                    false).Length != 0);
  }

  public static IEnumerable<Type> typeConstructors(this IEnumerable<Type> exports)
  {
    yield break;
    // return exports.Where(x => x.GetCustomAttributes(typeof(Runtime.TypeClassAttribute), 
    //                                                 false).Length != 0);
  }

}


public class Qualifier : EmptyVisitor
{
  List<Assembly> placesToLook = new List<Assembly>();
  // List<Type> typeClasses = new List<Type>();
  // List<Type> typeConstructors = new List<Type>();

  static class Binding
  {
    public static Binding<T> Create<T>(HsQName name, T value)
    {
      return new Binding<T> { Name = name, Value = value, };
    }
  }
  class Binding<T>
  {
    public HsQName Name { get; set; }
    public T Value { get; set; }
  }

  List<Binding<Type>> nameBindings = new List<Binding<Type>>();

  public Qualifier()
  {
    placesToLook.Add(typeof(Prelude).Assembly);
  }

  public override void visit(HsModule node)
  {
    // Import Prelude if it's not imported explicitly
    var module = node.Imports.FirstOrDefault(x => x.Module.Name == "Prelude");

    if(module == null)
      node.Imports.Add(new HsImportDecl {
          Location = node.Location,
          Module = new DOM.Module {
            Name = "Prelude"
          },
          Specs = new List<HsImportSpec>(),
        });
  }

  public override void visit(HsTyCon node)
  {
    // if(node.Name is UnQual)
    Console.WriteLine("Found type constructor. Name => {0}", node.Name);
    //ResolveTypeConstructor()
  }

  public override void visit(HsImportDecl node)
  {
    Console.WriteLine("Importing module: {0}", node.Module.Name);
    // This code somewhy fails NUnit
    node.ModuleDeclaringType = placesToLook
      .Select(x => x.findModule(node.Module.Name))
      .FirstOrDefault(x => x != null);

    // foreach(var asm in placesToLook)
    // {
    //   var module = asm.findModule(node.Module.Name);
    //   if(module != null) 
    //   {
    //     node.ModuleDeclaringType = module;
    //     break;
    //   }
    // }

    if (node.ModuleDeclaringType == null)
      throw new UnableToFindModuleException(node.Location, node.Module.Name);
    
    // IEnumerable<Type> importList
    if(node.Specs.Count == 0)
    {
      // Import everything...
      

      // var modTypeClasses = node.ModuleDeclaringType
      //   .allExports();
      // typeClasses.AddRange(modTypeClasses);

      // Console.WriteLine("{0} type classes found", typeClasses.Count());
    }
  }
  // public override void visit(UnQual node)
  // {
  //   Console.WriteLine("Found unqualified node. Name => {0}", node.Name.Name);
  // }
}

}

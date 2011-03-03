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

public enum DeclarationType
{
  Nothing,
  FunctionSet,
  TypeClass,
  ADT,
  Constructor,
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

  public static IEnumerable<MemberInfo> getMembers(this Type type)
  {
    switch(type.classify())
    {
      case DeclarationType.TypeClass:
      case DeclarationType.ADT:
        yield return type; break;
      case DeclarationType.FunctionSet:
        foreach(var fn in type.GetMethods(BindingFlags.Public | BindingFlags.Static))
          yield return fn;
        break;
      default: break;
    }
    yield break;
  }

  public static IEnumerable<MemberInfo> allExports(this Type module)
  {
    // TODO: Assert module is a Module
    return module
      .GetCustomAttributes(typeof(Runtime.ExportAttribute), false)
      .OfType<Runtime.ExportAttribute>()
      .SelectMany(x => x.Type.getMembers());

    // Console.WriteLine("Found {0} exports", retVal.Count());

    // return retVal;
  }

  public static string clearName(this string name)
  {
    var genericIdx = name.LastIndexOf('`');
    if(genericIdx != -1)
      return name.Remove(genericIdx);
    
    return name;
  }

  public static HsName haskellName(this Type type)
  {
    var nameAttr = type.GetCustomAttributes(typeof(Runtime.NameAttribute), 
                                            false);
    if(nameAttr.Length != 0)
      return new HsName { Name = (nameAttr[0] as Runtime.NameAttribute).Value, };
    else
      return new HsName { Name = type.Name.clearName(), };
  }

  public static HsName haskellName(this MethodInfo method)
  {
    var nameAttr = method.GetCustomAttributes(typeof(Runtime.NameAttribute), 
                                            false);
    if(nameAttr.Length != 0)
      return new HsName { Name = (nameAttr[0] as Runtime.NameAttribute).Value, };
    else
      return new HsName { Name = method.Name.clearName(), };
  }

  public static DeclarationType classify(this Type type)
  {
    if(type.GetCustomAttributes(typeof(Runtime.TypeClassAttribute),
                                false).Length != 0)
    {
      return DeclarationType.TypeClass;
    }

    if(type.GetCustomAttributes(typeof(Runtime.FunctionSetAttribute),
                                false).Length != 0)
    {
      return DeclarationType.FunctionSet;
    }

    if(type.GetCustomAttributes(typeof(Runtime.ConstructorAttribute),
                                false).Length != 0)
    {
      return DeclarationType.ADT;
    }
    return DeclarationType.Nothing;
  }

  public static HsImportSpec toImportSpec(this MemberInfo member)
  {
    if(member is Type)
      return new HsIAbs
      {
        Name = (member as Type).haskellName(),
      };
    else if(member is MethodInfo)
      return new HsIVar
      {
        Name = (member as MethodInfo).haskellName(),
      };
    else throw new Exception("ICE!");
  }

  // public static IEnumerable<HsImportSpec> toImportSpecs(this Type type)
  // {
  //   switch(type.classify())
  //   {
  //     case DeclarationType.TypeClass:
  //     case DeclarationType.ADT:
  //       yield return new HsIAbs
  //       {
  //         Name = type.haskellName(),
  //       };
  //       break;
  //     case DeclarationType.FunctionSet:
  //       foreach(var fn in type.GetMethods(BindingFlags.Public | BindingFlags.Static))
  //       {
  //         yield return new HsIVar
  //         {
  //           Name = fn.haskellName(),
  //         };
  //       }
  //       break;
  //     default: break;
  //   }
  //   yield break;
  // }

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

  List<Binding<MemberInfo>> nameBindings = new List<Binding<MemberInfo>>();

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
          Parent = node,
          Location = node.Location,
          Module = new DOM.Module {
            Name = "Prelude"
          },
          Specs = new List<HsImportSpec>(),
        });

    node.NameLookup = nameBindings.ToLookup(x => x.Name, x => x.Value);
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

    // This code fails NUnit on mono-4.0 target
    node.ModuleDeclaringType = placesToLook
      .Select(x => x.findModule(node.Module.Name))
      .FirstOrDefault(x => x != null);

    if (node.ModuleDeclaringType == null)
      throw new UnableToFindModuleException(node.Location, node.Module.Name);
    
    if(node.Specs.Count == 0)
    {
      node.Specs.AddRange(node.ModuleDeclaringType
                          .allExports()
                          .Select(x => x.toImportSpec().reparent(node)));
      // Import everything...
      //node.Specs.Add()

      // var modTypeClasses = node.ModuleDeclaringType
      //   .allExports();
      // typeClasses.AddRange(modTypeClasses);

      // Console.WriteLine("{0} type classes found", typeClasses.Count());
    }
    
    // foreach(var spec in nodeSpecs)
    //   spec.accept(this);
  }
  
  public override void visit(HsIVar node)
  {
    var importingModule = node.findParentOfType<HsImportDecl>();
    // importingModule.ModuleDeclaringType.funtion
    // nameBindings.Add(new Binding<MemberInfo>
    //                  {
                       
    //                  });
    Console.WriteLine("Importing function {0} from module {1}", node.Name, importingModule.Module.Name);
  }

  public override void visit(HsIAbs node)
  {
    var importingModule = node.findParentOfType<HsImportDecl>();
    
    // nameBindings.Add(importingModule.createBinding(m => {
    //       var exported = m.ModuleDeclaringType.allExports().Where(x => x.classify() in { ... , ..., ... }); 
    //       .Where(x => x.
    //       return null;
    //     });

    Console.WriteLine("Importing {0} from module {1}", node.Name, importingModule.Module.Name);
  }

  // public override void visit(UnQual node)
  // {
  //   Console.WriteLine("Found unqualified node. Name => {0}", node.Name.Name);
  // }
}

}

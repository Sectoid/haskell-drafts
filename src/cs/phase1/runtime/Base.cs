// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;

namespace Language.Haskell.Phase1.Runtime
{

#region Attributes

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = true)]
public class ImportAttribute : Attribute 
{
  public Type Type { get; set; }
  public ImportAttribute(Type t) { Type = t; }
}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = true)]
public class ExportAttribute : Attribute 
{
  public Type Type { get; set; }
  public ExportAttribute(Type t) { Type = t; }
}


[AttributeUsage(AttributeTargets.Method, 
                Inherited = false,
                AllowMultiple = false)]
public class NameAttribute : Attribute 
{
  public NameAttribute(string name) {}
}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = false)]
public class ADTAttribute : Attribute {}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = false)]
public class ADTCtorAttribute : Attribute {}

[AttributeUsage(AttributeTargets.Interface,
                Inherited = false,
                AllowMultiple = false)]
public sealed class TypeClassAttribute : Attribute {}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = false)]
public sealed class InstanceAttribute : Attribute 
{
  public InstanceAttribute(Type typeClass) {}
}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = true,
                AllowMultiple = false)]
public sealed class MarkTypeAttribute : Attribute 
{
  public MarkTypeAttribute(Type mark) {}
}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = true,
                AllowMultiple = false)]
public sealed class AliasOfAttribute : Attribute 
{
  public AliasOfAttribute(Type mark) {}
}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = true)]
public sealed class ConstructorAttribute : Attribute 
{
  public ConstructorAttribute(Type ctorType) {}
}

#endregion

#region Special "marker" and support types

public interface TyApp<T1, T2> {}

public abstract class Module {}

#endregion

}

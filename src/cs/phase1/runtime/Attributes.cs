// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;

namespace Language.Haskell.Phase1.Runtime
{

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = false)]
public class ADTAttribute : Attribute {}

[AttributeUsage(AttributeTargets.Class, 
                Inherited = false,
                AllowMultiple = false)]
public class ADTCtorAttribute : Attribute {}

}
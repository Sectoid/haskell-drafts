using System;

namespace Language.Haskell.Runtime
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

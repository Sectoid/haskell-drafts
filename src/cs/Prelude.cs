// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;

using Language.Haskell.Runtime;

namespace Prelude
{

[ADT]
[ADTCtor]
public class List<T> 
{
  public Tuple<T, List<T>> Items;
    
  public List(T v1, List<T> v2)
  { Items = System.Tuple.Create(v1, v2); }

  protected List() {} // FIXME: Hack
}

[ADTCtor]
public class Nil<T> : List<T> {}

[ADT]
[ADTCtor]
public class String : List<Char> {}

[ADT]
public abstract class Maybe<T> {}

[ADTCtor]
public class Just<T> : Maybe<T>
{
  public Tuple<T> Items;
  public Just(T v1)
  { Items = System.Tuple.Create(v1); }
}

[ADTCtor]
public class Nothing<T> : Maybe<T> {}

}

// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;

using Language.Haskell.Runtime;

namespace Prelude
{

[ADT]
[ADTCtor]
public class List<T> 
{
    public T Head;
    public List<T> Tail;
    
    public List(T head, List<T> tail)
    {
      Head = head;
      Tail = tail;
    }

    protected List() {} // FIXME: Hack
}

[ADTCtor]
public class Nil<T> : List<T> {}

[ADT]
[ADTCtor]
public class String : List<Char> {}

}

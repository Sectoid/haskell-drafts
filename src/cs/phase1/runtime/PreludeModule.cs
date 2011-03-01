// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using System;
using HsRuntime = Language.Haskell.Phase1.Runtime;

[HsRuntime.Import(typeof(HsRuntime.Prelude.Show<>))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.Monad<>))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.Maybe<>))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.Functor<>))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.List<>))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.Monad_Maybe))]
[HsRuntime.Import(typeof(HsRuntime.Prelude.Eq_List<>))]
public class Prelude : HsRuntime.Module
{
  public static void error(String message)
  {
    throw new Exception(message);
  }
}

namespace Language.Haskell.Phase1.Runtime.Prelude
{

[TypeClass]
public interface Monad<T>
{
  TyApp<T, A> @return<A>(A value);
  TyApp<T, V> bind<U,V>(TyApp<T, U> value, Func<U, TyApp<T, V>> f);
}

public interface Functor<T>
{
  TyApp<T, B> fmap<A,B>(Func<A, B> f, TyApp<T, A> value);
}

[TypeClass]
public interface Show<T>
{
  String show(T value);
}

[TypeClass]
public interface Eq<T>
{
  // [Name("==")]
  bool eq(T l, T r);
  // [Name("/=")]
  bool nEq(T l, T r);
}

public interface Ord<T> : Eq<T>
{
}

public class Maybe {};

[Constructor(typeof(Just<>))]
[Constructor(typeof(Nothing<>))]
public abstract class Maybe<T> : Maybe, TyApp<Maybe, T> {}

public class Just<T> : Maybe<T> 
{
  public T Value;
}

public class Nothing<T> : Maybe<T> {}

public class Monad_Maybe : Monad<Maybe>
{
  public Maybe<A> @return<A>(A value) 
  {
    return new Just<A> { Value = value, };
  }

  public Maybe<V> bind<U,V>(Maybe<U> value, Func<U, Maybe<V>> f)
  {
    if(value is Nothing<U>)
      return new Nothing<V>();
    else
      return f((value as Just<U>).Value);
  }

  TyApp<Maybe, A> Monad<Maybe>.@return<A>(A value) { return @return(value); }
  TyApp<Maybe, V> Monad<Maybe>.bind<U,V>(TyApp<Maybe, U> value, Func<U, TyApp<Maybe, V>> f)
  {
    if(!(value is Maybe<U>)) global::Prelude.error("");
    if(!(f is Func<U, Maybe<V>>)) global::Prelude.error("");

    return bind((Maybe<U>)value, (Func<U, Maybe<V>>)f); 
  }
}

public abstract class List {}

[Constructor(typeof(Cons<>))]
[Constructor(typeof(Nil<>))]
public abstract class List<T> : List, TyApp<List, T> {}

public class Nil<T> : List<T> {}

public class Cons<T> : List<T> 
{
  public T Head { get; set; }
  public List<T> Tail { get; set; }
}

public class Eq_List<T> : Eq<List<T>>
{
  Eq<T> instance1;
  public Eq_List(Eq<T> instance1)
  {
    this.instance1 = instance1;
  }

  // [Name("==")]
  public bool eq(List<T> l, List<T> r)
  {
    return false;
  }

  // [Name("/=")]
  public bool nEq(List<T> l, List<T> r)
  {
    return false;
  }
}

// public static class A
// {
//   public static String f<T>(Monad<T> instance, T value)
//   {
//     var t = instance.@return(value);
//     return null;
//   }

//   public static String g<T>(T value)
//   {
//     var rhs = new Just<T> { Value = value, };
//     return f(new Monad_Maybe(), rhs);
//   }

//   public static String h()
//   {
//     return g(1);
//   }

//   public static List<T> sort<Context, T>(Ord<T> instance, List<T> value)
//   {
//     return value;
//   }
// }

}


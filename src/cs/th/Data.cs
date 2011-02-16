// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using Prelude;
using Language.Haskell.Runtime;

namespace Language.Haskell.TH.Syntax
{

// -- These Names should never appear in a binding position in a TH syntax tree
// data Name = Name OccName NameFlavour deriving (Typeable, Data)
[ADT]
[ADTCtor]
public class Name
{
  public System.Tuple<OccName, NameFlavour> Items;

  public Name(OccName v1,
              NameFlavour v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// data NameFlavour
[ADT]
public abstract class NameFlavour {}

//   = NameS           -- ^ An unqualified name; dynamically bound
[ADTCtor]
public class NameS : NameFlavour {}

//   | NameQ ModName   -- ^ A qualified name; dynamically bound
[ADTCtor]
public class NameQ : NameFlavour
{
  public System.Tuple<ModName> Items;
  public NameQ(ModName v1) { Items = System.Tuple.Create(v1); }
}

//   | NameU Int#      -- ^ A unique local name
[ADTCtor]
public class NameU : NameFlavour
{
  public System.Tuple<int> Items;
  public NameU(int v1) { Items = System.Tuple.Create(v1); }
}


//   | NameL Int#      -- ^ Local name bound outside of the TH AST
[ADTCtor]
public class NameL : NameFlavour
{
  public System.Tuple<int> Items;
  public NameL(int v1) { Items = System.Tuple.Create(v1); }
}

//   | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
[ADTCtor]
public class NameG : NameFlavour
{
  public System.Tuple<NameSpace, PkgName, ModName> Items;
  public NameG(NameSpace v1, PkgName v2, ModName v3)
  {
    Items = System.Tuple.Create(v1, v2, v3);
  }
}

// newtype ModName = ModName String	-- Module name
[ADT]
[ADTCtor]
public class ModName
{
  public System.Tuple<String> Items;
  public ModName(String v1)
  {
    Items = System.Tuple.Create(v1);
  }
}

// newtype PkgName = PkgName String	-- package name
[ADT]
[ADTCtor]
public class PkgName
{
  public System.Tuple<String> Items;
  public PkgName(String v1)
  {
    Items = System.Tuple.Create(v1);
  }
}

// newtype OccName = OccName String
[ADT]
[ADTCtor]
public class OccName
{
  public System.Tuple<String> Items;
  public OccName(String v1)
  {
    Items = System.Tuple.Create(v1);
  }
}

// data NameSpace
[ADT]
public abstract class NameSpace {}

//             = VarName	-- ^ Variables
[ADTCtor]
public class VarName : NameSpace {}

//             | DataName	-- ^ Data constructors
[ADTCtor]
public class DataName : NameSpace {}

//             | TcClsName	-- ^ Type constructors and classes; Haskell has them
//                              -- in the same name space for now.
[ADTCtor]
public class TcClsName : NameSpace {}

#region data Dec

// data Dec
[ADT]
public abstract class Dec {}

// FunD Name [Clause]  -- { f p1 p2 = b where decs }
[ADTCtor]
public class FunD : Dec
{
  public System.Tuple<Name, List<Clause>> Items;
  public FunD(Name v1, List<Clause> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// ValD Pat Body [Dec]	{ p = b where decs }
[ADTCtor]
public class ValD : Dec
{
  public System.Tuple<Pat, Body, List<Dec>> Items;

  public ValD(Pat v1, Body v2, List<Dec> v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// DataD Cxt Name [TyVarBndr] [Con] [Name]     { data Cxt x => T x = A x | B (T x)  deriving (Z,W)}
[ADTCtor]
public class DataD : Dec
{
  public System.Tuple<Cxt, Name, List<TyVarBndr>,
    List<Con>, List<Name>> Items;

  public DataD(Cxt v1, Name v2, List<TyVarBndr> v3,
               List<Con> v4, List<Name> v5)
  { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// NewtypeD Cxt Name [TyVarBndr] Con [Name]	{ newtype Cxt x => T x = A (B x)       deriving (Z,W)}
[ADTCtor]
public class NewtypeD : Dec
{
  public System.Tuple<Cxt, Name, List<TyVarBndr>,
    Con, List<Name>> Items;

  public NewtypeD(Cxt v1, Name v2, List<TyVarBndr> v3,
                  Con v4, List<Name> v5)
  { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// TySynD Name [TyVarBndr] Type	{ type T x = (x,x) }
[ADTCtor]
public class TySynD : Dec
{
  public System.Tuple<Name, List<TyVarBndr>, Type> Items;

  public TySynD(Name v1, List<TyVarBndr> v2, Type v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// ClassD Cxt Name [TyVarBndr] [FunDep] [Dec]	{ class Eq a => Ord a where ds }
[ADTCtor]
public class ClassD : Dec
{
  public System.Tuple<Cxt, Name, List<TyVarBndr>, 
    List<FunDep>, List<Dec>> Items;

  public ClassD(Cxt v1, Name v2, List<TyVarBndr> v3,
                List<FunDep> v4, List<Dec> v5)
  { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// // InstanceD Cxt Type [Dec]	{ instance Show w => Show [w]       where ds }
[ADTCtor]
public class InstanceD : Dec
{
  public System.Tuple<Cxt, Type, List<Dec>> Items;
  public InstanceD(Cxt v1, Type v2, List<Dec> v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// SigD Name Type	{ length :: [a] -> Int }
[ADTCtor]
public class SigD : Dec
{
  public System.Tuple<Name, Type> Items;
  public SigD(Name v1, Type v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// ForeignD Foreign
[ADTCtor]
public class ForeignD : Dec
{
  public System.Tuple<Foreign> Items;
  public ForeignD(Foreign v1)
  { Items = System.Tuple.Create(v1); }
}
// PragmaD Pragma	{ {--} }
[ADTCtor]
public class PragmaD : Dec
{
  public System.Tuple<Pragma> Items;
  public PragmaD(Pragma v1)
  { Items = System.Tuple.Create(v1); }
}

// FamilyD FamFlavour Name [TyVarBndr] (Maybe Kind)	{ type family T a b c :: * }
[ADTCtor]
public class FamilyD : Dec
{
  public System.Tuple<FamFlavour, Name, 
    List<TyVarBndr>, Maybe<Kind>> Items;
  public FamilyD(FamFlavour v1, Name v2, 
                 List<TyVarBndr> v3, Maybe<Kind> v4)
  { Items = System.Tuple.Create(v1, v2, v3, v4); }
}

// DataInstD Cxt Name [Type] [Con] [Name] { data instance Cxt x => T [x] = A x 
//                                                                       | B (T x)
//                                               deriving (Z,W) }
[ADTCtor]
public class DataInstD : Dec
{
  public System.Tuple<Cxt, Name, 
    List<Type>, List<Con>, List<Name>> Items;
  public DataInstD(Cxt v1, Name v2,
                   List<Type> v3, List<Con> v4,
                   List<Name> v5)
  { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// NewtypeInstD Cxt Name [Type] Con [Name] { newtype instance Cxt x => T [x] = A (B x)
//                                                   deriving (Z,W)}
[ADTCtor]
public class NewtypeInstD : Dec
{
  public System.Tuple<Cxt, Name,
    List<Type>, Con, List<Name>> Items;
  public NewtypeInstD(Cxt v1, Name v2,
                      List<Type> v3, Con v4,
                      List<Name> v5)
  { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// TySynInstD Name [Type] Type { type instance T (Maybe x) = (x,x) }
[ADTCtor]
public class TySynInstD 
{
  public System.Tuple<Name, List<Type>, Type> Items;
  public TySynInstD(Name v1, List<Type> v2, Type v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

#endregion

#region data Exp

// data Exp
[ADT]
public abstract class Exp {}

// VarE Name { x }
[ADTCtor]
public class VarE : Exp
{
  public System.Tuple<Name> Items;
  public VarE(Name v1)
  { Items = System.Tuple.Create(v1); }
}

// ConE Name -- data T1 = C1 t1 t2; p = {C1} e1 e2
[ADTCtor]
public class ConE : Exp
{
  public System.Tuple<Name> Items;
  public ConE(Name v1)
  { Items = System.Tuple.Create(v1); }
}

// LitE Lit -- { 5 or c}
[ADTCtor]
public class LitE : Exp
{
  public System.Tuple<Lit> Items;
  public LitE(Lit v1)
  { Items = System.Tuple.Create(v1); }
}

// AppE Exp Exp -- { f x }
[ADTCtor]
public class AppE : Exp
{
  public System.Tuple<Exp,Exp> Items;
  public AppE(Exp v1, Exp v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// InfixE (Maybe Exp) Exp (Maybe Exp) -- {x + y} or {(x+)} or {(+ x)} or {(+)}
[ADTCtor]
public class InfixE : Exp
{
  public System.Tuple<Maybe<Exp>, Exp, Maybe<Exp>> Items;
  public InfixE(Maybe<Exp> v1, Exp v2, Maybe<Exp> v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// LamE [Pat] Exp -- {  p1 p2 -> e }
[ADTCtor]
public class LamE : Exp
{
  public System.Tuple<List<Pat>, Exp> Items;
  public LamE(List<Pat> v1, Exp v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// TupE [Exp] -- { (e1,e2) }
[ADTCtor]
public class TupE : Exp
{
  public System.Tuple<List<Exp>> Items;
  public TupE(List<Exp> v1)
  { Items = System.Tuple.Create(v1); }
}

// CondE Exp Exp Exp -- { if e1 then e2 else e3 }
[ADTCtor]
public class CondE : Exp
{
  public System.Tuple<Exp, Exp, Exp> Items;
  public CondE(Exp v1, Exp v2, Exp v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// LetE [Dec] Exp -- { let x=e1;   y=e2 in e3 }
[ADTCtor]
public class LetE : Exp
{
  public System.Tuple<List<Dec>, Exp> Items;
  public LetE(List<Dec> v1, Exp v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// CaseE Exp [Match] -- { case e of m1; m2 }
[ADTCtor]
public class CaseE : Exp
{
  public System.Tuple<Exp, List<Match>> Items;
  public CaseE(Exp v1, List<Match> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// DoE [Stmt] -- { do { p <- e1; e2 }  }
[ADTCtor]
public class DoE : Exp
{
  public System.Tuple<List<Stmt>> Items;
  public DoE(List<Stmt> v1)
  { Items = System.Tuple.Create(v1); }
}

// ArithSeqE Range	{ [ 1 ,2 .. 10 ] }
[ADTCtor]
public class ArithSeqE : Exp
{
  public System.Tuple<Range> Items;
  public ArithSeqE(Range v1)
  { Items = System.Tuple.Create(v1); }
}

// ListE [Exp] -- { [1,2,3] }
[ADTCtor]
public class ListE : Exp
{
  public System.Tuple<List<Exp>> Items;
  public ListE(List<Exp> v1)
  { Items = System.Tuple.Create(v1); }
}

// SigE Exp Type -- { e :: t }
[ADTCtor]
public class SigE : Exp
{
  public System.Tuple<Exp, Type> Items;
  public SigE(Exp v1, Type v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// RecConE Name [FieldExp] -- { T { x = y, z = w } }
[ADTCtor]
public class RecConE : Exp
{
  public System.Tuple<Name, List<FieldExp>> Items;
  public RecConE(Name v1, List<FieldExp> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// RecUpdE Exp [FieldExp] -- { (f x) { z = w } }
[ADTCtor]
public class RecUpdE : Exp
{
  public System.Tuple<Exp, List<FieldExp>> Items;
  public RecUpdE(Exp v1, List<FieldExp> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

#endregion

#region data Con
// data Con
[ADT]
public abstract class Con {}

// NormalC Name [StrictType] --  C Int a
[ADTCtor]
public class NormalC : Con
{
  public System.Tuple<Name, List<StrictType>> Items;
  public NormalC(Name v1, List<StrictType> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// RecC Name [VarStrictType] -- C { v :: Int, w :: a }
[ADTCtor]
public class RecC : Con
{
  public System.Tuple<Name, List<VarStrictType>> Items;
  public RecC(Name v1, List<VarStrictType> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// InfixC StrictType Name StrictType -- Int :+ a
[ADTCtor]
public class InfixC : Con
{
  public System.Tuple<StrictType, Name, StrictType> Items;
  public InfixC(StrictType v1, Name v2, StrictType v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// ForallC [TyVarBndr] Cxt Con -- forall a. Eq a => C [a]
[ADTCtor]
public class ForallC : Con
{
  public System.Tuple<List<TyVarBndr>, Cxt, Con> Items;
  public ForallC(List<TyVarBndr> v1, Cxt v2, Con v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

#endregion

#region data Type

// data Type
[ADT]
public abstract class Type {}

// ForallT [TyVarBndr] Cxt Type -- forall vars. ctxt -> type
[ADTCtor]
public class ForallT : Type
{
  public System.Tuple<List<TyVarBndr>, Cxt, Type> Items;
  public ForallT(List<TyVarBndr> v1, Cxt v2, Type v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

// VarT Name -- a
[ADTCtor]
public class VarT : Type
{
  public System.Tuple<Name> Items;
  public VarT(Name v1)
  { Items = System.Tuple.Create(v1); }
}

// ConT Name -- T
[ADTCtor]
public class ConT : Type
{
  public System.Tuple<Name> Items;
  public ConT(Name v1)
  { Items = System.Tuple.Create(v1); }
}

// TupleT Int -- (,), (,,), etc.
[ADTCtor]
public class TupleT : Type
{
  public System.Tuple<int> Items;
  public TupleT(int v1)
  { Items = System.Tuple.Create(v1); }    
}

// ArrowT -- ->
[ADTCtor]
public class ArrowT : Type {}

// ListT -- []
[ADTCtor]
public class ListT : Type {}

// AppT Type Type -- T a b
[ADTCtor]
public class AppT : Type
{
  public System.Tuple<Type, Type> Items;
  public AppT(Type v1, Type v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// SigT Type Kind -- t :: k
[ADTCtor]
public class SigT : Type
{
  public System.Tuple<Type, Kind> Items;
  public SigT(Type v1, Kind v2)
  { Items = System.Tuple.Create(v1, v2); }
}

#endregion

#region data TyVarBndr

// data TyVarBndr
[ADT]
public abstract class TyVarBndr {}

// PlainTV Name -- a
[ADTCtor]
public class PlainTV : TyVarBndr
{
  public System.Tuple<Name> Items;
  public PlainTV(Name v1)
  { Items = System.Tuple.Create(v1); }
}

// KindedTV Name Kind -- (a :: k)
[ADTCtor]
public class KindedTV : TyVarBndr
{
  public System.Tuple<Name, Kind> Items;
  public KindedTV(Name v1, Kind v2)
  { Items = System.Tuple.Create(v1, v2); }
}

#endregion

#region data Kind

// data Kind
[ADT]
public abstract class Kind {}

// StarK -- *
[ADTCtor]
public class StarK : Kind {}

// ArrowK Kind Kind  -- k1 -> k2
[ADTCtor]
public class ArrowK : Kind
{
  public System.Tuple<Kind, Kind> Items;
  public ArrowK(Kind v1, Kind v2)
  { Items = System.Tuple.Create(v1, v2); }
}

#endregion

#region data Pred

// data Pred
[ADT]
public abstract class Pred {}

// ClassP Name [Type] -- Eq (Int, a)
[ADTCtor]
public class ClassP : Pred
{
  public System.Tuple<Name, List<Type>> Items;
  public ClassP(Name v1, List<Type> v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// EqualP Type Type -- F a ~ Bool
[ADTCtor]
public class EqualP : Pred
{
  public System.Tuple<Type, Type> Items;
  public EqualP(Type v1, Type v2)
  { Items = System.Tuple.Create(v1, v2); }
}

#endregion

#region data Match

// Match Pat Body [Dec] -- case e of { pat -> body where decs }
[ADT]
[ADTCtor]
public class Match
{
  public System.Tuple<Pat, Body, List<Dec>> Items;
  public Match(Pat v1, Body v2, List<Dec> v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

#endregion

#region data Clause

// Clause [Pat] Body [Dec]	f { p1 p2 = body where decs }
[ADT]
[ADTCtor]
public class Clause 
{
  public System.Tuple<List<Pat>, Body, List<Dec>> Items;
  public Clause(List<Pat> v1, Body v2, List<Dec> v3)
  { Items = System.Tuple.Create(v1, v2, v3); }
}

#endregion

#region data Body

// data Body
[ADT]
public abstract class Body {}


// GuardedB [(Guard, Exp)]	-- f p { | e1 = e2 | e3 = e4 } where ds
[ADTCtor]
public class GuardedB : Body
{
  public System.Tuple<List<System.Tuple<Guard, Exp>>> Items;
  public GuardedB(List<System.Tuple<Guard, Exp>> v1)
  { Items = System.Tuple.Create(v1); }
}

// NormalB Exp -- f p { = e } where ds
[ADTCtor]
public class NormalB : Body
{
  public System.Tuple<Exp> Items;
  public NormalB(Exp v1)
  { Items = System.Tuple.Create(v1); }
}

#endregion

#region data Guard

// data Guard
[ADT]
public abstract class Guard {}


// NormalG Exp	
[ADTCtor]
public class NormalG : Guard
{
  public System.Tuple<Exp> Items;
  public NormalG(Exp v1)
  { Items = System.Tuple.Create(v1); }
}

// PatG [Stmt]	 
[ADTCtor]
public class PatG : Guard
{
  public System.Tuple<List<Stmt>> Items;
  public PatG(List<Stmt> v1)
  { Items = System.Tuple.Create(v1); }
}

#endregion

#region data Stmt

// data Stmt
[ADT]
public abstract class Stmt {}


// BindS Pat Exp
[ADTCtor]
public class BindS : Stmt
{
  public System.Tuple<Pat, Exp> Items;
  public BindS(Pat v1, Exp v2)
  { Items = System.Tuple.Create(v1, v2); }
}

// LetS [Dec]
[ADTCtor]
public class LetS : Stmt
{
  public System.Tuple<List<Dec>> Items;
  public LetS(List<Dec> v1)
  { Items = System.Tuple.Create(v1); }
}

// NoBindS Exp
[ADTCtor]
public class NoBindS : Stmt
{
  public System.Tuple<Exp> Items;
  public NoBindS(Exp v1)
  { Items = System.Tuple.Create(v1); }
}

// ParS [[Stmt]]	 
[ADTCtor]
public class ParS : Stmt
{
  public System.Tuple<List<List<Stmt>>> Items;
  public ParS(List<List<Stmt>> v1)
  { Items = System.Tuple.Create(v1); }
}

#endregion


// TODO

[ADT]
public abstract class Pat {}

[ADT]
public abstract class Cxt {}

[ADT]
public abstract class FunDep {}

[ADT]
public abstract class Foreign {}

[ADT]
public abstract class Pragma {}

[ADT]
public abstract class FamFlavour {}

[ADT]
public abstract class Lit {}

[ADT]
public abstract class Range {}

[ADT]
public abstract class FieldExp {}

[ADT]
public abstract class StrictType {}

[ADT]
public abstract class VarStrictType {}

}

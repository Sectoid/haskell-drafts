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
    public System.Tuple<Ctx, Name, List<TyVarBndr>,
                        List<Con>, List<Name>> Items;

    public DataD(Ctx v1, Name v2, List<TyVarBndr> v3,
                 List<Con> v4, List<Name> v5)
    { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// NewtypeD Cxt Name [TyVarBndr] Con [Name]	{ newtype Cxt x => T x = A (B x)       deriving (Z,W)}
[ADTCtor]
public class NewtypeD : Dec
{
    public System.Tuple<Ctx, Name, List<TyVarBndr>,
                        Con, List<Name>> Items;

    public NewtypeD(Ctx v1, Name v2, List<TyVarBndr> v3,
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
    public System.Tuple<Ctx, Name, List<TyVarBndr>, 
                        List<FunDep>, List<Dec>> Items;

    public ClassD(Ctx v1, Name v2, List<TyVarBndr> v3,
                  List<FunDep> v4, List<Dec> v5)
    { Items = System.Tuple.Create(v1, v2, v3, v4, v5); }
}

// // InstanceD Cxt Type [Dec]	{ instance Show w => Show [w]       where ds }
[ADTCtor]
public class InstanceD : Dec
{
    public System.Tuple<Ctx, Type, List<Dec>> Items;
    public InstanceD(Ctx v1, Type v2, List<Dec> v3)
    { Items = System.Tuple.Create(v1, v2, v3); }
}

// TODO
[ADT]
public abstract class Clause {}

[ADT]
public abstract class Pat {}

[ADT]
public abstract class Body {}

[ADT]
public abstract class Ctx {}

[ADT]
public abstract class Con {}

[ADT]
public abstract class TyVarBndr {}

[ADT]
public abstract class Type {}

[ADT]
public abstract class FunDep {}

}

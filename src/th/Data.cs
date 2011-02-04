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
    public OccName OccName;
    public NameFlavour NameFlavour;

    public Name(OccName occName,
                NameFlavour nf)
    {
      OccName = occName;
      NameFlavour = nf;
    }
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
    public ModName ModName;
    public NameQ(ModName modName) { ModName = modName; }
}

//   | NameU Int#      -- ^ A unique local name
[ADTCtor]
public class NameU : NameFlavour 
{ 
    public int Id;
    public NameU(int id) { Id = id; }
}


//   | NameL Int#      -- ^ Local name bound outside of the TH AST
[ADTCtor]
public class NameL : NameFlavour 
{ 
    public int Id;
    public NameL(int id) { Id = id; }
}

//   | NameG NameSpace PkgName ModName -- ^ Global name bound outside of the TH AST:
[ADTCtor]
public class NameG : NameFlavour 
{
    public NameSpace NameSpace;
    public PkgName PkgName;
    public ModName ModName;

    public NameG(NameSpace ns,
                 PkgName pkg,
                 ModName mod)
    {
      NameSpace = ns;
      PkgName = pkg;
      ModName = mod;
    }
}

// newtype ModName = ModName String	-- Module name
[ADT]
[ADTCtor]
public class ModName 
{
    public String String;
    public ModName(String name)
    {
      String = name;
    }
}

// newtype PkgName = PkgName String	-- package name
[ADT]
[ADTCtor]
public class PkgName 
{
    public String String;
    public PkgName(String name)
    {
      String = name;
    }
}

// newtype OccName = OccName String
[ADT]
[ADTCtor]
public class OccName 
{
    public String String;
    public OccName(String name)
    {
      String = name;
    }
}



// data NameSpace
[ADT]
public abstract class NameSpace {}

//             = VarName	-- ^ Variables
[ADTCtor]
public class VarName : NameSpace {}

// 	       | DataName	-- ^ Data constructors 
[ADTCtor]
public class DataName : NameSpace {}

// 	       | TcClsName	-- ^ Type constructors and classes; Haskell has them
// 				-- in the same name space for now.
[ADTCtor]
public class TcClsName : NameSpace {}

// // data Dec
// [ADT]
// public abstract class Dec {}

// // FunD Name [Clause]  -- { f p1 p2 = b where decs }
// [ADTCtor]
// public class FunD : Dec
// {
//     public Name Name;
//     public List<Clause> Clauses;
//     public FunD(Name name, List<Clause> clauses)
//     {
//       Name = name;
//       Clauses = clauses;
//     }
// }

// // ValD Pat Body [Dec]	{ p = b where decs }
// [ADTCtor]
// public class ValD : Dec
// {
//     public Pat Pat;
//     public Body Body;
//     public List<Dec> Decs;
    
//     public ValD(Pat pat,
// 		Body body,
// 		List<Dec> decs)
//     {
//       Pat = pat;
//       Body = body;
//       Decs = decs;
//     }
// }

// // DataD Cxt Name [TyVarBndr] [Con] [Name]     { data Cxt x => T x = A x | B (T x)  deriving (Z,W)}
// [ADTCtor]
// public class DataD : Dec
// {
//     public Cxt Ctx;
//     public Name Name;
//     public List<TyVarBndr> TyVarBndrs;
//     public List<Con> Cons;
//     public List<Name> Names;

//     public DataD(Cxt ctx,
// 		 Name name,
// 		 List<TyVarBndr> tyVarBndrs,
// 		 List<Con> cons,
// 		 List<Name> names)
//     {
//       Ctx = ctx;
//       Name = name;
//       TyVarBndrs = tyVarBndrs;
//       Cons = cons;
//       Names = names;
//     }
// }

// // NewtypeD Cxt Name [TyVarBndr] Con [Name]	{ newtype Cxt x => T x = A (B x)       deriving (Z,W)}
// [ADTCtor]
// public class NewtypeD : Dec
// {
//     public Cxt Ctx;
//     public Name Name;
//     public List<TyVarBndr> TyVarBndrs;
//     public Con Con;
//     public List<Name> Names;

//     public NewtypeD(Cxt ctx,
// 		    Name name,
// 		    List<TyVarBndr> tyVarBndrs,
// 		    Con con,
// 		    List<Name> names)
//     {
//       Ctx = ctx;
//       Name = name;
//       TyVarBndrs = tyVarBndrs;
//       Con = con;
//       Names = names;
//     }
// }

// // TySynD Name [TyVarBndr] Type	{ type T x = (x,x) }
// [ADTCtor]
// public class TySynD : Dec
// {
//     public Name Name;
//     public List<TyVarBndr> TyVarBndrs;
//     public Type Type;

//     public TySynD(Name name,
// 		  List<TyVarBndr> tyVarBndrs,
// 		  Type type)
//     {
//       Name = name;
//       TyVarBndrs = tyVarBndrs;
//       Type = type;
//     }
// }

// // ClassD Cxt Name [TyVarBndr] [FunDep] [Dec]	{ class Eq a => Ord a where ds }
// [ADTCtor]
// public class ClassD : Dec
// {
//     public Ctx Ctx;
//     public Name Name;
//     public List<TyVarBndr> TyVarBndrs;
//     public List<FunDep> FunDeps;
//     public List<Dec> Decs;

//     public ClassD(Ctx ctx,
// 		  Name name,
// 		  List<TyVarBndr> tyVarBndrs,
// 		  List<FunDep> funDeps,
// 		  List<Dec> decs)
//     {
//       Ctx = ctx;
//       Name = name;
//       TyVarBndrs = tyVarBndrs;
//       FunDeps = funDeps;
//       Decs = decs;
//     }
// }

// // // InstanceD Cxt Type [Dec]	{ instance Show w => Show [w]       where ds }
// // [ADTCtor]
// // public class InstanceD




// TODO
[ADT]
public abstract class Clause {}

}

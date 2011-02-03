using System;

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
    public ModName(string name)
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
    public OccName(string name)
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

}

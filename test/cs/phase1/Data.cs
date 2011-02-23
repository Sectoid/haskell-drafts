// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using NUnit.Framework;

namespace Language.Haskell.Phase1
{

internal static class TestData
{
  public static readonly string XmlAST = @"<?xml version='1.0' encoding='utf-8'?>
<HsModule xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/2001/XMLSchema'>
  <!-- this declaration are futher skipped in the sample, but they have to be generated in the common scheme -->
  <Location><SrcLoc srcLine='1' srcColumn='1' /></Location>
  <Module>Crash</Module>
  <Body>
    <HsTypeSig>
      <Names>
	<HsName>main</HsName>
      </Names>
      <Type>
	<Context>
	  <Assertions></Assertions>
	</Context>
	<Type xsi:type='HsTyApp'>
	  <First xsi:type='HsTyCon'>
	    <Name xsi:type='UnQual'>IO</Name>
	  </First>
	  <Second xsi:type='HsTyCon'>
	    <Name xsi:type='Special'>
	      <Value xsi:type='HsUnitCon' />
	    </Name>
	  </Second>
	</Type>
      </Type>
    </HsTypeSig>
    <HsPatBind>
      <Pattern xsi:type='HsPVar'>
	<Name>
	  <HsName>main</HsName>
	</Name>
      </Pattern>
      <Rhs xsi:type='HsUnGuardedRhs'>
	<Expression xsi:type='HsInfixApp'>
	  <Left xsi:type='HsVar'>
	    <Name xsi:type='UnQual'>print</Name>
	  </Left>
	  <Op xsi:type='HsQVarOp'>
	    <Name xsi:type='UnQual'>$</Name>
	  </Op>
	  <Right xsi:type='HsApp'>
	    <Left xsi:type='HsApp'>
	      <Left xsi:type='HsVar'>
		<Name xsi:type='UnQual'>map</Name>
	      </Left>
	      <Right xsi:type='HsRightSection'>
		<Op xsi:type='HsQVarOp'>
		  <Name xsi:type='UnQual'>*</Name>
		</Op>
		<Right xsi:type='HsLit'>
		  <Value xsi:type='HsInt'>2</Value>
		</Right>
	      </Right>
	    </Left>
	    <Right xsi:type='HsEnumFromTo'>
	      <From xsi:type='HsLit'>
		<Value xsi:type='HsInt'>1</Value>
	      </From>
	      <To xsi:type='HsLit'>
		<Value xsi:type='HsInt'>3</Value>
	      </To>
	    </Right>
	  </Right>
	</Expression>
      </Rhs>
    </HsPatBind>
  </Body>
</HsModule>";
}

}
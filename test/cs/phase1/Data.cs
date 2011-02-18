// -*- mode: csharp; indent-tabs-mode: nil; c-basic-offset: 2; -*-

using NUnit.Framework;

namespace Language.Haskell.Phase1
{

internal static class TestData
{
  public static readonly string XmlAST = @"<?xml version='1.0' encoding='utf-8'?>
<HsModule>
  <!-- this declaration are futher skipped in the sample, but they have to be generated in the common scheme -->
  <Location><SrcLoc srcLine='1' srcColumn='1' /></Location>
  <Module>Crash</Module>
  <Body>
    <HsTypeSig>
      <Names>
	<HsName>main</HsName>
      </Names>
      <Type>
	<HsQualType>
	  <Context />
	  <Type>
	    <HsTyApp>
	      <First>
		<HsTyCon>
		  <Name>
		    <UnQual>IO</UnQual>
		  </Name>
		</HsTyCon>
	      </First>
	      <Second>
		<HsTyCon>
		  <Name>
		    <Special>IO</Special>
		  </Name>
		</HsTyCon>
	      </Second>
	    </HsTyApp>
	  </Type>
	</HsQualType>
      </Type>
    </HsTypeSig>
    <HsPatBind>
      <Pattern>
	<HsPVar>
	  <Name>
	    <HsName>main</HsName>
	  </Name>
	</HsPVar>
      </Pattern>
      <Rhs>
	<HsUnGuardedRhs>
	  <Expression>
	    <HsInfixApp>
	      <Left>
		<HsVar>
		  <Name><UnQual>print</UnQual></Name>
		</HsVar>
	      </Left>
	      <Op>
		<HsQVarOp>
		  <Name><UnQual>$</UnQual></Name>
		</HsQVarOp>
	      </Op>
	      <Right>
		<HsApp>
		  <Left>
		    <HsApp>
		      <Left>
			<HsVar>
			  <Name><UnQual>map</UnQual></Name>
			</HsVar>
		      </Left>
		      <Right>
			<HsRightSection>
			  <Op>
			    <HsQVarOp>
			      <Name><UnQual>*</UnQual></Name>
			    </HsQVarOp>
			  </Op>
			  <Right>
			    <HsLit>
			      <Value><HsInt>2</HsInt></Value>
			    </HsLit>
			  </Right>
			</HsRightSection>
		      </Right>
		    </HsApp>
		  </Left>
		  <Right>
		    <HsEnumFromTo>
		      <From>
			<HsLit>
			  <Value><HsInt>1</HsInt></Value>
			</HsLit>
		      </From>
		      <To>
			<HsLit>
			  <Value><HsInt>3</HsInt></Value>
			</HsLit>
		      </To>
		    </HsEnumFromTo>
		  </Right>
		</HsApp>
	      </Right>
	    </HsInfixApp>
	  </Expression>
	</HsUnGuardedRhs>
      </Rhs>
    </HsPatBind>
  </Body>
</HsModule>";
}

}

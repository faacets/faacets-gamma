package com.faacets.core

import com.faacets.FaacetsSuite
import com.faacets.laws.{DataLaws, Exprs}
import net.alasc.laws.AnyRefLaws
import org.scalacheck.Arbitrary

class LawTests extends FaacetsSuite {

  import com.faacets.laws.Exprs._
  import com.faacets.laws.Scenarios.Small._

//  nestedCheckAll[Scenario]("Expr", Scenario.CHSH) { scenario =>
    //DataLaws[Expr[scenario.type]](implicitly, Arbitrary(Exprs.genExpr(scenario))).coded
    //AnyRefLaws[Expr[scenario.type]]._eq
//  }

}

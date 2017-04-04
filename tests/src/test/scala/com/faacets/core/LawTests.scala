package com.faacets.core

import com.faacets.FaacetsSuite
import com.faacets.laws.DataLaws

class LawTests extends FaacetsSuite {

  import com.faacets.laws.Scenarios.Small._
  import com.faacets.laws.Exprs.arbExpr

  checkAll("Expr", DataLaws[Expr].coded)

}

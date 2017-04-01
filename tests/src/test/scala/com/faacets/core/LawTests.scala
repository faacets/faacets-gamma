package com.faacets.core

import com.faacets.FaacetsSuite
import com.faacets.laws.DataLaws
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import scalin.immutable.Vec
import spire.algebra.Eq
import spire.math.{Rational, SafeLong}

class LawTests extends FaacetsSuite {

  import com.faacets.laws.Scenarios.Small._
  import com.faacets.laws.Exprs.arbExpr

  checkAll("Expr", DataLaws[Expr].coded)

}

package com.faacets.operation

import com.faacets.FaacetsSuite
import com.faacets.core._
import scalin.immutable.Vec
import spire.math.Rational
import scalin.immutable.dense._

class BarneaSuite extends FaacetsSuite {

    test("Two inequalities contributed by T. Barnea are equivalent.") {
      val s = "[(2 2) (3 3 3) (2 2)]".parseUnsafe[Scenario]
      val i1 = Expr.collinsGisin(s,
        Vec[Rational](-1,1,-1,1,-1,1,-1,1,-1,0,0,0,-2,-2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,-2,0,0,-2,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,0,0))
      val i2 = Expr.collinsGisin(s,
        Vec[Rational](-1,1,-1,0,0,0,0,0,0,1,-1,1,-1,1,-1,0,0,0,-2,-2,2,-2,0,0,0,0,0,0,0,0,
          -2,0,0,2,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
      val r1 = OperationExtractor[Expr, Relabeling].forceExtract(i1).operation
      val r2 = OperationExtractor[Expr, Relabeling].forceExtract(i2).operation
      val j1 = i1 <|+| r1
      val j2 = i2 <|+| r2
      j1 should ===(j2)
  }

}

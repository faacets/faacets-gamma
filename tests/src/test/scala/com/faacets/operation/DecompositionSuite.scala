package com.faacets.operation

import com.faacets.FaacetsSuite
import com.faacets.core.Expr
import com.faacets.laws._
import com.faacets.operation.Extracted.CanonicalWithAffine

class DecompositionSuite extends FaacetsSuite {

  import Operations.genCanonicalWithAffine
  import Affines.affineGenerator
  import Liftings.liftingGenerator
  import Reorderings.reorderingGenerator
  import Relabelings.relabelingGenerator

  forAll { c: Canonical[Expr] =>
    forAll(genCanonicalWithAffine(c.value)) { ld =>
      CanonicalWithAffineExtractor.forV[Expr].apply(ld.original).canonical === c.value
    }
  }

  test("Consideration of the opposite expression") {
    val posDec = CanonicalWithAffineExtractor.forV[Expr].apply(Expr.I3322)
    val negDec = CanonicalWithAffineExtractor.forV[Expr].apply(-Expr.I3322)
    assert(posDec.original === Expr.I3322)
    assert(negDec.original === (-Expr.I3322))
    assert(posDec.affine.multiplier.signum == -negDec.affine.multiplier.signum)
    assert(posDec.canonical === negDec.canonical)
  }

}

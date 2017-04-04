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
      CanonicalWithAffine(ld.original).canonical === c.value
    }
  }

}

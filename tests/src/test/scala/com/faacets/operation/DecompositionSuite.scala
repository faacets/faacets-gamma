package com.faacets.operation

import com.faacets.FaacetsSuite
import com.faacets.core.Expr
import com.faacets.laws._

class DecompositionSuite extends FaacetsSuite {

  import LinearDecompositions.genLinearDecomposition
  import Affines.affineGenerator
  import Liftings.liftingGenerator
  import Reorderings.reorderingGenerator
  import Relabelings.relabelingGenerator

  forAll { c: Canonical[Expr] =>
    forAll(genLinearDecomposition(c.value)) { ld =>
      LinearDecomposition(ld.original).canonical === c.value
    }
  }

}

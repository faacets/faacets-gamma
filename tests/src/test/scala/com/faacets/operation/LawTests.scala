package com.faacets
package operation

import net.alasc.laws.AnyRefLaws

import com.faacets.core.{Expr, Relabeling}
import com.faacets.laws.{DataLaws, OperationLaws}
import com.faacets.operation.lifting.Grouping

class LawTests extends FaacetsSuite {

  {
    import com.faacets.laws.Groupings._
    import com.faacets.laws.Scenarios.Large._
    checkAll("Grouping", DataLaws[Grouping].textable)
    checkAll("Grouping", AnyRefLaws[Grouping]._eq)
  }

  locally {
    import com.faacets.laws.Affines.Positive.arbAffine
    import com.faacets.laws.Affines.{affineCloner, affineGenerator, affineInstances}
    import com.faacets.laws.Exprs.arbExpr
    import com.faacets.laws.Scenarios.Large.arbScenario
    checkAll("Affine", DataLaws[Affine].textable)
    checkAll("Affine", AnyRefLaws[Affine]._eq)
    checkAll("Affine", OperationLaws[Expr, Affine].groupoid)
  }

  locally {
    import com.faacets.laws.Exprs.arbExpr
    import com.faacets.laws.Reorderings.{arbReordering, reorderingCloner, reorderingGenerator, reorderingInstances}
    import com.faacets.laws.Scenarios.Large._
    checkAll("Reordering", DataLaws[Reordering].textable)
    checkAll("Reordering", AnyRefLaws[Reordering]._eq)
    checkAll("Reordering", OperationLaws[Expr, Reordering].groupoid)
  }

  locally {
    import com.faacets.laws.Exprs.arbExpr
    import com.faacets.laws.Liftings.{arbLifting, liftingCloner, liftingGenerator, liftingInstances}
    import com.faacets.laws.Scenarios.Small._
    checkAll("Lifting", DataLaws[Lifting].textable)
    checkAll("Lifting", AnyRefLaws[Lifting]._eq)
    checkAll("Lifting", OperationLaws[Expr, Lifting].groupoid)
  }

  locally {
    import com.faacets.laws.Exprs.arbExpr
    import com.faacets.laws.Relabelings.{arbRelabeling, relabelingGenerator}
    import com.faacets.laws.Scenarios.Small._
    checkAll("Relabeling", OperationLaws[Expr, Relabeling].groupoid)
  }

}

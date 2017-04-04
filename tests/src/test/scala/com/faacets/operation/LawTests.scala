package com.faacets
package operation

import com.faacets.core.{Expr, Relabeling}
import com.faacets.laws.{DataLaws, OperationLaws}
import com.faacets.operation.lifting.Grouping
import net.alasc.laws.AnyRefLaws

/* TODO
import scala.reflect.ClassTag
import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.laws._

*/
// import scala.{specialized => spec}
import org.typelevel.discipline.scalatest.Discipline
// import org.scalacheck.{Arbitrary, Gen, Prop}
// import org.typelevel.discipline.Laws
import org.scalatest.FunSuite

class LawTests extends FunSuite with Discipline {

  import com.faacets.laws.Groupings._
  import com.faacets.laws.Liftings._
  import com.faacets.laws.Reorderings._


  {
    import com.faacets.laws.Scenarios.Large._
    checkAll("Grouping", DataLaws[Grouping].textable)
    checkAll("Grouping", AnyRefLaws[Grouping]._eq)
  }

  locally {
    import com.faacets.laws.Affines.{affineCloner, affineInstances, affineGenerator}
    import com.faacets.laws.Affines.Positive.arbAffine
    import com.faacets.laws.Scenarios.Large.arbScenario
    import com.faacets.laws.Exprs.arbExpr
    checkAll("Affine", DataLaws[Affine].textable)
    checkAll("Affine", AnyRefLaws[Affine]._eq)
    checkAll("Affine", OperationLaws[Expr, Affine].groupoid)
  }

  locally {
    import com.faacets.laws.Reorderings.{arbReordering, reorderingCloner, reorderingInstances, reorderingGenerator}
    import com.faacets.laws.Scenarios.Large._
    import com.faacets.laws.Exprs.arbExpr
    checkAll("Reordering", DataLaws[Reordering].textable)
    checkAll("Reordering", AnyRefLaws[Reordering]._eq)
    checkAll("Reordering", OperationLaws[Expr, Reordering].groupoid)
  }

  locally {
    import com.faacets.laws.Liftings.{arbLifting, liftingInstances, liftingCloner, liftingGenerator}
    import com.faacets.laws.Scenarios.Small._
    import com.faacets.laws.Exprs.arbExpr
    checkAll("Lifting", DataLaws[Lifting].textable)
    checkAll("Lifting", AnyRefLaws[Lifting]._eq)
    checkAll("Lifting", OperationLaws[Expr, Lifting].groupoid)
  }

  locally {
    import com.faacets.operation.instances.expr._
    import com.faacets.laws.Relabelings.{arbRelabeling, relabelingGenerator}
    import com.faacets.laws.Scenarios.Small._
    import com.faacets.laws.Exprs.arbExpr
    checkAll("Relabeling", OperationLaws[Expr, Relabeling].groupoid)
  }

}

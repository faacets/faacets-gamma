package com.faacets
package operation

import com.faacets.laws.DataLaws
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

  //checkAll("Lifting", AnyRefLaws[Lifting]._eq)

  locally {
    import com.faacets.laws.Affines.{affineCloner, affineInstances}
    import com.faacets.laws.Affines.Positive.arbAffine
    checkAll("Affine", DataLaws[Affine].textable)
    checkAll("Affine", AnyRefLaws[Affine]._eq)
    //  checkAll("Affine", OperationLaws[Expr, Affine].groupoid)
  }

  locally {
    import com.faacets.laws.Reorderings.{arbReordering, reorderingCloner, reorderingInstances}
    import com.faacets.laws.Scenarios.Large._
    checkAll("Reordering", DataLaws[Reordering].textable)
    checkAll("Reordering", AnyRefLaws[Reordering]._eq)
//    checkAll("Reordering", OperationLaws[Expr, Reordering].groupoid)
  }

  locally {
    import com.faacets.laws.Liftings.{arbLifting, liftingInstances, liftingCloner}
    import com.faacets.laws.Scenarios.Large._
    checkAll("Lifting", DataLaws[Lifting].textable)
    checkAll("Lifting", AnyRefLaws[Lifting]._eq)
//    checkAll("Lifting", OperationLaws[Expr, Lifting].groupoid)
  }

  locally {
//    import Relabelings.{arbRelabeling, relabelingGenerator, relabelingInstances, relabelingCloner}
//    checkAll("Relabeling", OperationLaws[Expr, Relabeling].groupoid)

  }

}

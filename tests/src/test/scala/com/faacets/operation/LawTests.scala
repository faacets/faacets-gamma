package com.faacets
package operation

import com.faacets.laws.DataLaws
import com.faacets.operation.lifting.Grouping
import net.alasc.laws.AnyRefLaws

import scala.reflect.ClassTag
import spire.algebra._
import spire.algebra.lattice._
import spire.math._
import spire.laws._

import scala.{specialized => spec}
import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.typelevel.discipline.Laws
import org.scalatest.FunSuite

class LawTests extends FunSuite with Discipline {

  import com.faacets.laws.Groupings._

  {
    import com.faacets.laws.Scenarios.Large._
    checkAll("Grouping", DataLaws[Grouping].textable)
    checkAll("Grouping", AnyRefLaws[Grouping]._eq)
  }

  /*
  import core.laws.Scenarios.Small._
  import core.laws.Parties.Small._
  import core.laws.Exprs.NonSignaling._

  import Liftings.{arbLifting, liftingGenerator,
    liftingInstances, liftingCloner}
  import Affines.{affineGenerator,
    affineInstances, affineCloner}
  import Affines.Mixed.arbAffine
  import Relabelings.{arbRelabeling, relabelingGenerator,
    relabelingInstances, relabelingCloner}
  import Reorderings.{arbReordering, reorderingGenerator,
    reorderingInstances, reorderingCloner}
  import Redundants.{arbRedundant, redundantGenerator,
    redundantInstances, redundantCloner}
  import RepresentationChanges.{arbRepresentationChange,
    representationChangeGenerator, representationChangeInstances,
    representationChangeCloner}

  checkAll("Lifting", DataLaws[Lifting].textable)
  checkAll("Lifting", AnyRefLaws[Lifting]._eq)
  checkAll("Lifting", OperationLaws[Expr, Lifting].groupoid)

  checkAll("Affine", DataLaws[Affine].textable)
  checkAll("Affine", AnyRefLaws[Affine]._eq)
  checkAll("Affine", OperationLaws[Expr, Affine].groupoid)

  checkAll("Relabeling", DataLaws[Relabeling].textable)
  checkAll("Relabeling", AnyRefLaws[Relabeling]._eq)
  checkAll("Relabeling", OperationLaws[Expr, Relabeling].groupoid)

  checkAll("Reordering", DataLaws[Reordering].textable)
  checkAll("Reordering", AnyRefLaws[Reordering]._eq)
  checkAll("Reordering", OperationLaws[Expr, Reordering].groupoid)
*/

}

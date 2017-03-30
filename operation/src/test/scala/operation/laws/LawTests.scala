package com.faacets
package operation
package laws
/*
import scala.reflect.ClassTag

import spire.algebra._
import spire.algebra.lattice._
import spire.math._

import spire.laws._

import scala.{ specialized => spec }

import org.typelevel.discipline.scalatest.Discipline
import org.scalacheck.{Gen, Arbitrary, Prop}
import org.typelevel.discipline.Laws

import org.scalatest.FunSuite

import net.alasc.math._
import net.alasc.laws._
import net.alasc.math.wreath._

import core.Expr
import core.perm.Relabeling
import data._
import core.laws._

class LawTests extends FunSuite with Discipline {
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

  {
    implicit def arbCanonical: Arbitrary[Canonical[Expr]] =
      Arbitrary(Canonical.genExpr.map(expr => Canonical(expr.to(expr.representation.wide))))

    checkAll("Redundant", OperationLaws[Expr, Redundant].groupoid)
    checkAll("Redundant", AnyRefLaws[Redundant]._eq)
    checkAll("Redundant", DataLaws[Redundant].format)
  }

  checkAll("RepresentationChange", OperationLaws[Expr, RepresentationChange].groupoid)
  checkAll("RepresentationChange", DataLaws[RepresentationChange].textable)
  checkAll("RepresentationChange", AnyRefLaws[RepresentationChange]._eq)
}
*/
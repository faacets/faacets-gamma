package com.faacets

import spire.algebra.Eq
import spire.syntax.EqOps

import org.scalacheck.Shrink
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

import net.alasc.laws.NestedDiscipline

/**
  * An opinionated stack of traits to improve consistency and reduce
  * boilerplate in Faacets tests (inspired by Cats).
  */
trait FaacetsSuite extends FunSuite with Matchers
    with PropertyChecks
    with spire.syntax.AllSyntax
    with scalin.syntax.AllSyntax
    with net.alasc.syntax.AllSyntax
    with data.syntax.AllSyntax
    with data.instances.AllInstances
    with core.syntax.AllSyntax
    with operation.syntax.AllSyntax
    with operation.instances.AllInstances
    with consolidate.syntax.AllSyntax
    with Discipline
    with NestedDiscipline
    with StrictFaacetsEquality {

  // disable Eq syntax (by making `eqOps` not implicit), since it collides
  // with scalactic's equality
  override def eqOps[A:Eq](a:A): EqOps[A] = new EqOps[A](a)

  def discardEvaluation(): Nothing = throw new DiscardedEvaluationException

  def noShrink[T] = Shrink[T](_ => Stream.empty)

}

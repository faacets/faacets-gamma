package com.faacets.core
package ref

import cats.data.{Validated, ValidatedNel}
import cats.instances.vector._
import cats.syntax.traverse._
import com.faacets.core.text.FullTerm

import spire.algebra.Action
import spire.math.Rational
import spire.syntax.action._
import spire.syntax.group._

case class BellOperator(elements: Map[Set[POVM], Rational])

final class BellOperatorRefRelabelingAction extends Action[BellOperator, RefRelabeling] {
  def actl(rr: RefRelabeling, bo: BellOperator): BellOperator = actr(bo, rr.inverse)
  def actr(bo: BellOperator, rr: RefRelabeling): BellOperator = BellOperator(bo.elements.map {
    case (k, v) => (k.map(_ <|+| rr), v)
  })
}

final class BellOperatorRelabelingAction extends Action[BellOperator, Relabeling] {
  def actl(r: Relabeling, bo: BellOperator): BellOperator = actr(bo, r.inverse)
  def actr(bo: BellOperator, r: Relabeling): BellOperator = BellOperator(bo.elements.map {
    case (k, v) => (k.map(_ <|+| r), v)
  })
}

object BellOperator {

  implicit val refRelabelingAction: Action[BellOperator, RefRelabeling] = new BellOperatorRefRelabelingAction
  implicit val relabelingAction: Action[BellOperator, Relabeling] = new BellOperatorRelabelingAction

  def fromGenExpr[V[X <: Scenario with Singleton] <: GenExpr[V, X], S <: Scenario with Singleton](genExpr: GenExpr[V, S]): BellOperator = {
    val elements = (0 until genExpr.coefficients.length).filterNot(genExpr.coefficients(_).isZero).map { ind =>
      val (aArray, xArray) = genExpr.scenario.ind2subP(ind)
      val key = (0 until genExpr.scenario.nParties).map(p => POVM(p, xArray(p), aArray(p))).toSet
      (key, genExpr.coefficients(ind))
    }
    BellOperator(elements.toMap)
  }

  def toDExpr(scenario: Scenario, bo: BellOperator): ValidatedNel[String, DExpr[scenario.type]] = {
    val terms: ValidatedNel[String, Vector[DExpr[scenario.type]]] = bo.elements.toVector.map {
      case (key, value) =>
        val maxP = key.map(_.p).fold(-1)(spire.math.max(_, _))
        if (maxP >= scenario.nParties)
          Validated.invalidNel(s"Party $maxP out of bounds")
        else {
          val outputsInputs: ValidatedNel[String, Vector[(Int, Int)]] = Vector.range(0, scenario.nParties).map { p =>
            val elements = key.filter(_.p == p)
            if (elements.size != 1) Validated.invalidNel("Each party should have a single POVM element")
            else {
              val povm = elements.head
              Validated.valid((povm.a, povm.x))
            }
          }.sequenceU
          val res: ValidatedNel[String, DExpr[scenario.type]] = outputsInputs.andThen { oi =>
            val (outputs, inputs) = oi.unzip
            FullTerm(outputs, inputs).validate(scenario).map(_ :* value)
          }
          res
        }
    }.sequenceU
    terms.map(_.fold(DExpr.zero(scenario))(_ + _))
  }

}

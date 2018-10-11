package com.faacets.core

import spire.implicits._
import spire.math.Rational

import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._

import com.faacets.polyta._

object NonSignalingPolytope {
  def hPolytope(scenario: Scenario): (HPolytope[Rational], Vec[Rational]) = {
    //implicit def action = scenario.probabilityAction
    //val generators = scenario.group.generators.map(g => g.to[Perm])
    //val group = Grp(generators.toSeq:_*)
    val ineqs = scenario.parties.map(p => p.matrices.matSPfromSC * p.matrices.matSCfromNC).foldLeft(Mat.ones[Rational](1, 1)) {
      case (lhs, rhs) => rhs kron lhs
    }
    val a = -ineqs(::, 1 until ineqs.nCols)
    val b = ineqs.apply(::, 0)
    val x0 = Vec.tabulate(a.nCols)(i => if (i == 0) Rational.one else Rational.zero)
    (HPolytope(a, b), x0)
  }
  def apply(scenario: Scenario): NonSignalingPolytope[scenario.type] = new NonSignalingPolytope(scenario)
}

class NonSignalingPolytope[S <: Scenario with Singleton](val scenario: S) {

  lazy val (hPolytope, validPoint) = NonSignalingPolytope.hPolytope(scenario)

  lazy val vPolytope = solvers.Porta.toVPolytope(hPolytope, validPoint)

  lazy val extremalBoxes: Vector[Behavior[S]] =
    Vector.tabulate(vPolytope.mV.nRows) { r =>
      val coeffs = Vec.tabulate(vPolytope.mV.nCols + 1) { c =>
        if (c == 0) Rational.one else vPolytope.mV(r, c - 1)
      }
      Behavior.correlators(scenario: S, coeffs)
    }

}

package com.faacets.core

import spire.implicits._
import spire.math.Rational

import scalin.immutable.{Mat, Vec}
import scalin.immutable.dense._

import com.faacets.polyta._

object LocalPolytope {
  def vPolytope(scenario: Scenario): VPolytope[Rational] = {
    //implicit def action = scenario.strategyAction
    //val generators = scenario.group.generators.map(g => g.to[Perm])
    //val group = Grp(generators.toSeq:_*)
    val mat = scenario.parties.map(p => p.matrices.matNCfromT * p.matrices.matTfromW).foldLeft(Mat.ones[Rational](1, 1)) {
      case (lhs, rhs) => rhs kron lhs
    }
    val extPoints = mat.t
    VPolytope(extPoints)
/*    if (useSymmetries)
      new SymVRepr(extPoints, rays, group)
    else
      new VRepr(extPoints, rays)*/
  }
  def apply(scenario: Scenario): LocalPolytope[scenario.type] = new LocalPolytope(scenario)
}

/** Class representing the local polytope for a Bell scenario. */
class LocalPolytope[S <: Scenario with Singleton](val scenario: S) {

  lazy val vPolytope = LocalPolytope.vPolytope(scenario)

  lazy val hPolytope = solvers.Porta.toHPolytope(vPolytope)

  /** Returns a list of local facets, as an expression and its rational upper bound. */
  lazy val facets: Vector[(Expr[S], Rational)] =
    Vector.tabulate(hPolytope.mA.nRows) { r =>
      val coeffs = hPolytope.mA(r, ::)
      val bound = hPolytope.vb(r)
      (Expr.correlators(scenario: S, coeffs), bound)
    }

}

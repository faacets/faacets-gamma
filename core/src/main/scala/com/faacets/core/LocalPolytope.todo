package com.faacets.core

/*
import spire.math.Rational
import spire.implicits._

import net.alasc.math.{Grp, Perm}
import net.alasc.syntax.permutationAction._
import qalg.immutable.{QMatrix, QVector}

import polyta._

object LocalPolytope {
  def vRepr(scenario: Scenario, useSymmetries: Boolean): VRepr = {
    implicit def action = scenario.strategyAction
    val generators = scenario.group.generators.map(g => g.to[Perm])
    val group = Grp(generators.toSeq:_*)
    val ncnp = scenario.matrices.corr(NCRepresentation, NPRepresentation)
    val npw = scenario.matrices.corr(NPRepresentation, WRepresentation)
    val extPoints: QMatrix = (ncnp * npw).t
    val rays = QMatrix.zeros(0, 0)
    if (useSymmetries)
      new SymVRepr(extPoints, rays, group)
    else
      new VRepr(extPoints, rays)
  }
  def apply(scenario: Scenario, useSymmetries: Boolean = true) = new LocalPolytope(scenario, useSymmetries)
}

/** Class representing the local polytope for a Bell scenario. */
class LocalPolytope(val scenario: Scenario, useSymmetries: Boolean) extends Polytope(None, Some(LocalPolytope.vRepr(scenario, useSymmetries)), "LP_" + scenario.toIdentifier) {
  /** Returns a list of local facets, as an expression and its rational upper bound. */
  def facets: Seq[(Expr, Rational)] = {
    (for (r <- 0 until hRepr.a.rows) yield {
      val expr = Expr(scenario, NCRepresentation,
        hRepr.a(r, ::).toQVector.toImmutable)
      (expr, hRepr.b(r))
    }).toSeq
  }
}
*/
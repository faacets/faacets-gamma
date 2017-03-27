package com.faacets.core


/*
import spire.implicits._
import spire.math.{Number, Rational}

import net.alasc.math.{Grp, Perm}
import net.alasc.syntax.permutationAction._

import qalg.immutable.{QMatrix => ImmutableQMatrix, QVector => ImmutableQVector}
import qalg.mutable.{QVector => MutableQVector}

import polyta._

object NonSignalingPolytope {
  def hRepr(scenario: Scenario, useSymmetries: Boolean): HRepr = {
    implicit def action = scenario.probabilityAction
    val generators = scenario.group.generators.map(g => g.to[Perm])
    val group = Grp(generators.toSeq:_*)
    val ineqs = scenario.matrices.corr(NPRepresentation, NCRepresentation)
    val a = -ineqs(::, 1 until ineqs.cols)
    val b = ineqs(::, 0).toQVector
    val aeq = ImmutableQMatrix.zeros(0, 0)
    val beq = ImmutableQVector.zeros(0)
    val x0 = ImmutableQVector.tabulate(a.cols)(i => if (i == 0) Rational.one else Rational.zero)
    if (useSymmetries)
      new SymHRepr(a, b, aeq, beq, group, Some(x0))
    else
      new HRepr(a, b, aeq, beq, Some(x0))
  }
  def apply(scenario: Scenario, useSymmetries: Boolean = true) = new NonSignalingPolytope(scenario, useSymmetries)
}

class NonSignalingPolytope(scenario: Scenario, useSymmetries: Boolean) extends Polytope(Some(NonSignalingPolytope.hRepr(scenario, useSymmetries)), None, "NS_" + scenario.toIdentifier) {
  def vertices: Seq[Corr] = {
    (for (r <- 0 until vRepr.e.rows) yield {
      val bx = MutableQVector.fill(vRepr.e.cols+1)(1)
      for (c <- 0 until vRepr.e.cols) bx(c+1) = vRepr.e(r, c)
      Corr(scenario, NCRepresentation, bx.toImmutable)
    }).toSeq
  }
}
*/
package com.faacets.core

/*
import spire.math.{Rational, SafeLong}

import scalin.immutable.Vec

import scalin.syntax.all._

import scalin.immutable.dense._

import spire.syntax.cfor._

final class LocalModel[S <: Scenario with Singleton](val scenario: S, val weights: Vec[Rational]) {

  def toCorr: Corr[S] = ???

}

object LocalModel {

  def apply(scenario: Scenario, weights: Vec[Rational]): LocalModel[scenario.type] =
    new LocalModel[scenario.type](scenario, weights)

  def uniformlyRandom(scenario: Scenario): LocalModel[scenario.type] = {
    val d = scenario.strategyRep.dimension
    new LocalModel[scenario.type](scenario, ones[Rational](d))

  }

  def tabulate(scenario: Scenario)(f: Array[Array[Int]] => Rational): LocalModel[scenario.type] = {
    val d = scenario.strategyRep.dimension
    val subs = Array.tabulate(scenario.nParties) { p =>
      Array.fill(scenario.parties(p).nInputs)(0)
    }
    val indP = Array.fill(scenario.nParties)(0)
    val weights = scalin.syntax.build.tabulate[Rational](d) { ind =>
      scenario.shape.primitivePrimitive.ind2sub(ind, indP)
      cforRange(0 until scenario.nParties) { p =>
        scenario.parties(p).shape.primitive.ind2sub(indP(p), subs(p))
      }
      f(subs)
    }
    new LocalModel[scenario.type](scenario, weights)
  }

}
*/
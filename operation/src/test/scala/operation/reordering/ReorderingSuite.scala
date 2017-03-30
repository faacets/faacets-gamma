package com.faacets
package operation
package reordering
/*
import org.scalatest.{FunSuite, NonImplicitAssertions}

import spire.syntax.action._
import spire.syntax.eq._

import net.alasc.math.Perm

import qalg.immutable.QVector

import core._
import data._
import core.perm.Relabeling

import syntax.all._

class ReorderingSuite extends FunSuite with NonImplicitAssertions {
  test("Simple reordering of a single party") {
    val source = "[(4 3 2)]".fromText[Scenario]
    val target = "[(3 4 2)]".fromText[Scenario]
    val reordering = Reordering(source, target)
    assert((source <|+| reordering) === target)
    assert(source.partialExtract[Reordering].isEmpty)
    val expr = Expr(source, SPRepresentation, QVector(4,4,4,4,3,3,3,2,2))
    assert((expr <|+| reordering).coefficients === QVector(3,3,3,4,4,4,4,2,2))
  }
}
*/
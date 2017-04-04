package com.faacets
package operation
package relabeling
/*
import org.scalatest.FunSuite
import org.scalacheck._

import spire.math.Rational

import qalg.immutable.QVector
import net.alasc.syntax.all._

import core._
import perm.Relabeling

class ExprRepresentativesSuite extends FunSuite {
  import operation.laws.Canonical.{chsh, positivity, mermin, cglmp3}

  test("Representatives of CHSH are in lexicographic order") {
    val representatives = chsh.representatives.iterator.toList
    for ( (before, after) <- (representatives zip representatives.drop(1)) ) {
      assert((0 until before.get.coefficients.length).exists { i =>
        before.get.coefficients(i) < after.get.coefficients(i) })
    }
  }

  test("Number of representatives is correct, and nTh is working for a few inequalities") {
    for (expr <- List(mermin, cglmp3, chsh)) {
      val reprBigSeq = expr.representatives
      val list1 = reprBigSeq.iterator.map(_.get).toList
      val list2 = (0 until reprBigSeq.size.toInt).map(k => reprBigSeq(k).get).toSeq
      assert(list1 == list2)
    }
  }
}
*/
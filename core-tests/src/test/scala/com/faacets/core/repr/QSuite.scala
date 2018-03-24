package com.faacets
package core
package repr

import scalin.immutable.Mat
import scalin.immutable.dense._
import spire.math.Rational

class QSuite extends FaacetsSuite {

  test("Q matrix and inverse for n = 2..12") {
    for (n <- 2 to 12)
      assert((Q(n).matrix*Q(n).matrixInverse) == Mat.eye[Rational](n))
  }
/*
  test("Labels for n = 5") {
    assert(Q(5).groups.map(_.toText).sameElements(List("0,1,2,3,4/", "5:0/1", "5:1/2", "5:2/3", "3/4")))
  }

  test("Groups for n = 6") {
    assert(Q(6).groups == List(
      Group(6, Set(0,1,2,3,4,5),Set.empty),
      Group(6, Set(0), Set(1)),
      Group(6, Set(1), Set(2)),
      Group(6, Set(2), Set(3)),
      Group(6, Set(3), Set(4)),
      Group(6, Set(4), Set(5)))
    )
  }*/

}

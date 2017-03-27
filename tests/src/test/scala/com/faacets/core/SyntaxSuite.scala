package com.faacets
package core

class SyntaxSuite extends FaacetsSuite {

  test("Relabeling string syntax") {

    (rel"(A, B)" |+| rel"A(0,1)") should ===(rel"B(0,1) (A,B)")

  }

}

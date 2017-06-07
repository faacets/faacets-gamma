package com.faacets
package data

import org.scalacheck.Prop
import org.scalatest.Inside

class ValueSuite extends FaacetsSuite with Inside {

  val valid =
    Table("bound",
      // exact values
      "2",
      "2/3",
      "2/3 - 1",
      "sqrt(2/3)",
      "cos(2*pi/10)",
      "sin(-3*pi/4)",
      "cos(-pi/4)",
      "cos(-pi/3) + 1/2",
      "3*sqrt(3) + 2*sqrt(5)/3",
      // implicit intervals
      "2 * 3.14",
      "2*3.14/3 - 3/4",
      "-3.14/5 + 1",
      "-2*3.14/4 + 4/5",
      // intervals
      "[1, 2.3]",
      "[2*sin(3*pi/2)/3, 101.4]",
      "[1, inf[",
      "]-inf,inf[",
      "[2*cos(pi/3)/5 + 3/2, 4/5 + 3/2]"
    )

  val invalid =
    Table("bound",
      "sqrt(3) + 2.3", // mixing non-rational cyclotomics and decimals is not allowed
      "inf", // infinity can only be part of an interval
      "cos(3)", // only rational multiples of pi are allowed
      "2 cos(3)", // the * operator is required
      "2.3 + 4.5", // arithmetic of decimals is not allowed
      "sqrt(-3)", // negative square root are imaginary and not allowed
      "2*[1, 2] + 3/4" // arithmetic on intervals is not supported
    )

  test("Valid examples") {
    forAll(valid) { (expr: String) =>
      expr.parse[Value].fold(_.toList, x => List.empty[String]) shouldBe List.empty[String]
    }
  }

  test("Invalid examples") {
    forAll(invalid) { (expr: String) =>
      expr.parse[Value].fold(_.toList, x => List.empty[String]) shouldNot be (List.empty[String])
    }
  }

}

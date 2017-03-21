package com.faacets
package core

import spire.math.SafeLong

class UniquenessCacheSuite extends FaacetsSuite {

  import UniquenessCacheSuite._

  test("Uniqueness cache memorizes created objects") {
    val a = Factorial(3)
    val b = Factorial(4)
    val c = Factorial(3)
    (a eq c) shouldBe true
    (a eq b) shouldBe false
  }

}

object UniquenessCacheSuite {

    final class Factorial private (val n: Long) {

    override def hashCode = n.hashCode
    override def toString = s"Factorial($n)"
    override def equals(any: Any) = any match {
      case that: Factorial => this eq that
      case _ => false
    }

    lazy val fact: SafeLong = spire.math.fact(n)

  }

  object Factorial extends UniquenessCache[Long, Factorial] {
    protected def valueFromKey(n: Long): Factorial = new Factorial(n)
    protected def keyFromValue(f: Factorial): Option[Long] = Some(f.n)
  }

}

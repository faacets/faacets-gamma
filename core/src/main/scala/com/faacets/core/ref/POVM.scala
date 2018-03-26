package com.faacets.core.ref

import spire.algebra.Eq

case class POVM(p: Int, x: Int, a: Int)

object POVM {
  implicit val equ: Eq[POVM] = Eq.fromUniversalEquals
}

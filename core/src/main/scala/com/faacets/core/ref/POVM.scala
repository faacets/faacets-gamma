package com.faacets.core.ref

import spire.algebra.Eq

/** Describes an element of a measurement in a Bell scenario, for the p-th party, x-th measurement, a-th outcome. */
case class POVM(p: Int, x: Int, a: Int)

object POVM {
  implicit val equ: Eq[POVM] = Eq.fromUniversalEquals
}

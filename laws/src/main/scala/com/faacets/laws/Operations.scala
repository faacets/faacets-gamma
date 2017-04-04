package com.faacets.laws

object Operations {

  import org.scalacheck.Gen

  case class Generator[V, O](gen: V => Gen[O])

}

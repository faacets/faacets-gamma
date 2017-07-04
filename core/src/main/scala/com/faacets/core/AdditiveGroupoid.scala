package com.faacets.core

import spire.algebra.partial.Groupoid

/** Groupoid with additive conventions. Used to add/define the zero for Bell expressions. */

case class AdditiveGroupoid[A](groupoid: Groupoid[A])

object AdditiveGroupoid {

  def apply[A](implicit ev: AdditiveGroupoid[A]): AdditiveGroupoid[A] = ev

}

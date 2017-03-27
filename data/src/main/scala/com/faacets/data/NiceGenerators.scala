package com.faacets.data

import spire.NoImplicit

import net.alasc.finite.Grp

/** Type-class to find "nice" generators for a given group. */
trait NiceGenerators[G] {

  def niceGenerators(grp: Grp[G]): Iterable[G]

}

object NiceGenerators {

  def apply[G](implicit G: NiceGenerators[G]): NiceGenerators[G] = G

  /** Default low-priority instance that just uses the current group generators. */
  implicit def stdNiceGenerators[G](implicit ev: NoImplicit[NiceGenerators[G]]): NiceGenerators[G] = new NiceGenerators[G] {

    def niceGenerators(grp: Grp[G]) = grp.generators

  }

}


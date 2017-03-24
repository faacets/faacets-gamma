package com.faacets.data.instances

import com.faacets.consolidate.Merge

import net.alasc.finite.Grp

// import net.alasc.finite.{Grp, GrpBuilder}

/** Type-class to find "nice" generators for a given group. */
trait NiceGenerators[G] {

  def niceGenerators(grp: Grp[G]): Iterable[G]

}

object NiceGenerators {

  def apply[G](implicit G: NiceGenerators[G]): NiceGenerators[G] = G

}

trait GrpInstances0 {

  /** Default low-priority instance that just uses the current group generators. */
  implicit def stdNiceGenerators[G]: NiceGenerators[G] = new NiceGenerators[G] {

    def niceGenerators(grp: Grp[G]) = grp.generators

  }

}

trait GrpInstances extends GrpInstances0 {

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

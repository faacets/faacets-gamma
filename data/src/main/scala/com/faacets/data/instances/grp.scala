package com.faacets.data.instances

import com.faacets.consolidate.Merge

import net.alasc.finite.Grp

trait GrpInstances {

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

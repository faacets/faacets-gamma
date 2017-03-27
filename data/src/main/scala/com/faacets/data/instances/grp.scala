package com.faacets.data.instances

import com.faacets.consolidate.Merge
import com.faacets.data.NiceGenerators
import io.circe.Encoder

import net.alasc.finite.Grp
import safeLong.safeLongEncoder

trait GrpInstances {

  implicit def grpEncoder[G:Encoder:NiceGenerators]: Encoder[Grp[G]] = Encoder.forProduct2("generators", "order")( grp => (NiceGenerators[G].niceGenerators(grp), grp.order) )

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

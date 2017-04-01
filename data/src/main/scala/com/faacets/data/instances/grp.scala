package com.faacets.data.instances

import com.faacets.consolidate.Merge
import com.faacets.data.NiceGenerators
import io.circe.{Decoder, Encoder}
import net.alasc.finite.{Grp, GrpGroup}
import safeLong._
import spire.math.SafeLong

trait GrpInstances {

  implicit def grpEncoder[G:Encoder:NiceGenerators]: Encoder[Grp[G]] = Encoder.forProduct2("generators", "order")( grp => (NiceGenerators[G].niceGenerators(grp), grp.order) )

  implicit def grpDecoder[G:Decoder:GrpGroup]: Decoder[Grp[G]] = Decoder.forProduct2("generators", "order")( (g: Vector[G], o: SafeLong) => Grp.fromGeneratorsAndOrder(g, o) )

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

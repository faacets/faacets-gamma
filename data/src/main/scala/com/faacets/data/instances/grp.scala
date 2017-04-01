package com.faacets.data.instances

import com.faacets.consolidate.Merge
import com.faacets.data.NiceGenerators
import io.circe.AccumulatingDecoder.Result
import io.circe.{AccumulatingDecoder, Decoder, Encoder, HCursor}
import net.alasc.finite.{Grp, GrpGroup}
import safeLong.safeLongEncoder
import spire.math.SafeLong

trait GrpInstances {

  implicit def grpEncoder[G:Encoder:NiceGenerators]: Encoder[Grp[G]] = Encoder.forProduct2("generators", "order")( grp => (NiceGenerators[G].niceGenerators(grp), grp.order) )

  /*implicit def grpDecoder[G:Decoder:GrpGroup]: AccumulatingDecoder[Grp[G]] = AccumulatingDecoder[Grp[G]].apply { c: HCursor =>
      AccumulatingDecoder.resultInstance.map2(c.get[Vector[G]]("generators"), c.get[BigInt])( (gens, order) => Grp.fromGeneratorsAndOrder(gens, SafeLong(order) ))
  }*/

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

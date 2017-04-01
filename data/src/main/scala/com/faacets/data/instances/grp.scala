package com.faacets.data.instances

import cats.data.Validated
import com.faacets.consolidate.Merge
import com.faacets.data.NiceGenerators
import io.circe._
import net.alasc.algebra.PermutationAction
import net.alasc.blackbox.RandomBag
import net.alasc.bsgs.{BaseGuide, GrpChainExplicit, GrpChainPermutationAction, KernelBuilder, MutableChain, SchreierSims}
import net.alasc.finite.{Grp, GrpGroup}
import safeLong._
import scalin.immutable.Vec
import spire.algebra.{Eq, Group}
import spire.math.{Rational, SafeLong}
import spire.util.Opt

import scala.reflect.ClassTag

trait GrpInstances {

  implicit def grpEncoder[G:Encoder:NiceGenerators]: Encoder[Grp[G]] = Encoder.forProduct2("generators", "order")( grp => (NiceGenerators[G].niceGenerators(grp), grp.order) )

  implicit def grpDecoder[G:ClassTag:Decoder:Eq:Group:GrpChainPermutationAction]: Decoder[Grp[G]] =  new Decoder[Grp[G]] {

    def apply(c: HCursor): Decoder.Result[Grp[G]] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[Grp[G]] =
      AccumulatingDecoder.resultInstance.map2(
        Decoder[Vector[G]].tryDecodeAccumulating(c.downField("generators")),
        Decoder[Option[SafeLong]].tryDecodeAccumulating(c.downField("order"))
      )( (_, _) ).andThen {
        case (gens: Vector[G], None) => Validated.valid(Grp.fromGenerators(gens))
        case (gens: Vector[G], Some(order)) =>
          val bag = RandomBag(gens, scala.util.Random)
          val action: PermutationAction[G] = GrpChainPermutationAction[G].faithfulAction(gens)
          implicit def a: action.type = action
          val baseStart = BaseGuide.Empty.baseAnsatz[G, action.type](gens)
          val mutableChain = MutableChain.emptyWithBase[G, action.type](baseStart)
          val kb = KernelBuilder.trivial[G]
          var tries = 0
          val maxTries = 32
          while (mutableChain.start.next.order < order && tries < maxTries) {
            if (SchreierSims.siftAndAddStrongGenerator(mutableChain, bag.apply(scala.util.Random), kb))
              tries = 0
            else
              tries += 1
          }
          val computedOrder = mutableChain.start.next.order
          if (computedOrder != order)
            Validated.invalidNel(DecodingFailure(s"Found group of $computedOrder, while given order is $order", Nil))
          else
            Validated.valid(new GrpChainExplicit[G, action.type](mutableChain.toChain(), Opt(gens), kb.toChain()))
      }

  }

  implicit def grpMerge[G]: Merge[Grp[G]] = Merge.fromEquals[Grp[G]] // TODO: Eq

}

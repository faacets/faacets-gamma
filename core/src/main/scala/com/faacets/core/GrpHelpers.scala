package com.faacets
package core


import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra._
import spire.syntax.group._
import spire.util.Opt
import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{BuildMutableChain, Chain, GrpChainPermutationAction, KernelBuilder, SchreierSims, SiftResult, Term}
import net.alasc.finite.{FaithfulPermutationActionBuilder, Grp}
import net.alasc.perms.default._

object GrpHelpers {

  def additionalGeneratorsFor[G]
  (grp: Grp[G], subgrp: Grp[G], conjugatedBy: Opt[Grp[G]] = Opt.empty[Grp[G]], rng: Random = scala.util.Random)
  (implicit builder: GrpChainPermutationAction[G], fpab: FaithfulPermutationActionBuilder[G]): Iterable[G] = {
    val action = fpab(grp)
    val grp0 = builder.fromGrp(grp, action)
    val subgrp0 = builder.fromGrp(subgrp, action)
    import builder.{equ, group, classTag}
    type F = action.type
    implicit def F: F = action
    val grpChain: Chain[G, F] = grp0.chain
    val subgrpChain: Chain[G, F] = subgrp0.chain
    additionalGeneratorsForChain(grpChain, subgrpChain, conjugatedBy, rng)
  }

  def additionalGeneratorsForChain[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (groupChain: Chain[G, F], subgroupChain: Chain[G, F], conjugatedBy: Opt[Grp[G]] = Opt.empty[Grp[G]], rng: Random = scala.util.Random)
  (implicit permutationAction: F): Iterable[G] = {
    val mutableChain = BuildMutableChain.fromChain[G, F, F](subgroupChain, Term.apply[G, F], KernelBuilder.trivial[G])
    var additionalGenerators = List.empty[G]
    val groupOrder = groupChain.order
    while (mutableChain.start.next.order < groupOrder) {
      def addToChainIfNew(g: G): Boolean = SchreierSims.siftAndUpdateBaseFrom(mutableChain, mutableChain.start, g) match {
        case SiftResult.Stop(generator, nodeForGenerator) =>
          mutableChain.addStrongGeneratorHere(nodeForGenerator, generator, generator.inverse)
          true
        case _ => false
      }
      val candidate = groupChain.randomElement(rng)
      if (addToChainIfNew(candidate)) {
        additionalGenerators :+= candidate
        conjugatedBy match {
          case Opt(cb) => cb.iterator.foreach { g => addToChainIfNew(g.inverse |+| candidate |+| g) }
          case _ =>
        }
      }
    }
    additionalGenerators
  }

}

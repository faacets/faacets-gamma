package com.faacets
package core

/*
import scala.reflect.ClassTag
import scala.util.Random

import spire.algebra._
import spire.math.{Rational, SafeLong}
import spire.syntax.group._
import spire.util.Opt
import spire.syntax.cfor._

import net.alasc.finite.{Grp, GrpChainBuilder, Rep}
import net.alasc.perms.{FaithfulPermRep, Perm}
import net.alasc.perms.default._
import net.alasc.syntax.all._
import perm.{ImprimitivePartyRelabelingRep, _}

import Rep.algebra._
import Rep.convert._
import net.alasc.algebra.{Permutation, PermutationAction}
import net.alasc.bsgs.{BuildMutableChain, Chain, GrpChain}

object GrpHelpers {

  def additionalGeneratorsFor[G]
  (grp: Grp[G], subgrp: Grp[G], conjugatedBy: Opt[Grp[G]] = Opt.empty[Grp[G]], rng: Random = scala.util.Random)
  (implicit builder: GrpChainBuilder[G]): Iterable[G] = {
    import builder.{equ, group, classTag}
    val GrpChain.AndAction(pair) = builder.fromGrp(grp)
    type F = pair.Action
    implicit def F: F = pair.action
    val grpChain: Chain[G, F] = pair.grp.chain
    val subgrpChain: Chain[G, F] = builder.convertGrp[F](subgrp, pair.grp.repOpt).chain
    additionalGeneratorsForChain(grpChain, subgrpChain, conjugatedBy, rng)
  }

  def additionalGeneratorsForChain[G:ClassTag:Eq:Group, F <: PermutationAction[G] with Singleton]
  (groupChain: Chain[G, F], subgroupChain: Chain[G, F], conjugatedBy: Opt[Grp[G]] = Opt.empty[Grp[G]], rng: Random = scala.util.Random)
  (implicit permutationAction: F): Iterable[G] = {
    val mutableChain = BuildMutableChain.fromChain[G, F, F](subgroupChain)
    var additionalGenerators = List.empty[G]
    val groupOrder = groupChain.order
    while (mutableChain.start.next.order < groupOrder) {
      def addToChainIfNew(g: G): Boolean = mutableChain.siftAndUpdateBaseFrom(mutableChain.start, g) match {
        case Opt((nodeForGenerator, generator)) =>
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
*/
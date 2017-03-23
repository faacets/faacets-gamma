package com.faacets
package core
package perm

/*
import scala.annotation.tailrec
import scala.util.Random
import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.{Permutation, PermutationAction}
import net.alasc.bsgs.{GrpChain, MutableChain, MutableStartOrNode, NodeBuilder, SchreierSims}
import net.alasc.finite.Grp
import net.alasc.syntax.permutationAction._
import net.alasc.bsgs._
import net.alasc.perms.FaithfulPermRep

object GrpLexAnsatz {

  def chainFromGeneratorsAndOrder[G:ClassTag:Eq:Group:NodeBuilder, F <: PermutationAction[G] with Singleton]
    (generators: IndexedSeq[G], order: SafeLong)
    (implicit permutationAction: F, schreierSims: SchreierSims): Chain[G, F] = {
    val mutableChain = MutableChain.empty[G, F]
    def rec(after: MutableStartOrNode[G, F], remaining: Iterable[G], startFrom: Int): Unit = {
      var beta = startFrom
      while (remaining.forall(g => beta == (beta <|+| g)))
        beta += 1
      val newNode = NodeBuilder[G].standalone(beta)
      mutableChain.insertInChain(after, after.next, newNode)
      val (generatorsThere, forNext) = remaining.partition(g => (beta <|+| g) != beta)
      if (!forNext.isEmpty)
        rec(newNode, forNext, beta + 1)
      generatorsThere.foreach( g => mutableChain.addStrongGeneratorHere(newNode, g, g.inverse) )
    }
    rec(mutableChain.start, generators, 0)
    if (mutableChain.start.next.order != order) {
      mutableChain.completeStrongGenerators
      if (mutableChain.start.next.order != order)
        throw new IllegalArgumentException(s"The group was provided a wrong order during construction, got ${mutableChain.start.next.order} instead of $order")
    }
    mutableChain.toChain()
  }

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group:NodeBuilder, K]
    (generators: IndexedSeq[G], order: SafeLong, rep: FaithfulPermRep[G, K])
    (implicit schreierSims: SchreierSims): GrpChainExplicit[G, rep.F] = {
    implicit def permutationAction: rep.F = rep.permutationAction
    val chain = chainFromGeneratorsAndOrder[G, rep.F](generators, order)
    new GrpChainExplicit[G, rep.F](chain, Opt(generators), Opt(rep))
  }

}
*/
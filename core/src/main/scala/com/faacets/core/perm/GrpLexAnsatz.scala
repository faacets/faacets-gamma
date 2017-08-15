package com.faacets
package core
package perm

import scala.reflect.ClassTag

import spire.algebra.{Eq, Group}
import spire.math.SafeLong
import spire.syntax.group._
import spire.syntax.action._
import spire.util.Opt

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{ Chain, KernelBuilder, MutableChain, MutableStartOrNode, NodeBuilder, SchreierSims, Term }
import net.alasc.bsgs.internal.GrpChainExplicit

object GrpLexAnsatz {

  def chainFromGeneratorsAndOrder[G:ClassTag:Eq:Group:NodeBuilder, F <: PermutationAction[G] with Singleton]
    (generators: IndexedSeq[G], order: SafeLong)
    (implicit permutationAction: F): Chain[G, F] = {
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
      SchreierSims.completeStrongGenerators(mutableChain, KernelBuilder.trivial)
      if (mutableChain.start.next.order != order)
        throw new IllegalArgumentException(s"The group was provided a wrong order during construction, got ${mutableChain.start.next.order} instead of $order")
    }
    mutableChain.toChain()
  }

  def fromGeneratorsAndOrder[G:ClassTag:Eq:Group:NodeBuilder]
    (generators: IndexedSeq[G], order: SafeLong, faithfulAction: PermutationAction[G]): GrpChainExplicit[G, faithfulAction.type] = {
    type F = faithfulAction.type
    require(faithfulAction.isFaithful)
    implicit def fa: F = faithfulAction
    val chain = chainFromGeneratorsAndOrder[G, F](generators, order)
    new GrpChainExplicit[G, F](chain, Opt(generators), Term.generic[G])
  }

}

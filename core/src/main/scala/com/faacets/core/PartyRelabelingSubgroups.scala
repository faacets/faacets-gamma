package com.faacets
package core

import com.faacets.core.perm.PartyShapeLattice

import net.alasc.algebra.PermutationAction
import net.alasc.bsgs.{GrpChain, GrpChainPermutationAction, SubgroupDefinition}
import net.alasc.finite.Grp
import net.alasc.perms.Perm
import net.alasc.perms.default._
import net.alasc.syntax.all._

case class PartyRelabelingSubgroups(val group: Grp[PartyRelabeling]) {

  val shapeLattice = PartyShapeLattice.fromPartyRelabelings(group.generators)

  val impShape = shapeLattice.shape.imprimitive

  implicit val action: PermutationAction[PartyRelabeling] = shapeLattice.shape.ImprimitiveAction

  val groupInRep: GrpChain[PartyRelabeling, action.type] = GrpChainPermutationAction[PartyRelabeling].fromGrp(group, action)

  val nInputs = shapeLattice.party.nInputs

  def subgroupFor(test: (Int, Int) => Boolean, predicate: PartyRelabeling => Boolean): GrpChain[PartyRelabeling, action.type] = {
    val definition = SubgroupDefinition[PartyRelabeling, action.type](test, predicate)(action)
    GrpChainPermutationAction[PartyRelabeling].subgroupFor[action.type](groupInRep, action, definition)
  }

  /** Returns the permutation group corresponding to pure input relabelings. */
  def inputsPermSubgroup: Grp[Perm] = {
    val subgrp = inputsSubgroup
    Grp.fromGeneratorsAndOrder(subgrp.generators.map(_.xPerm), subgrp.order)
  }

  /** Returns the subgroup of pure input relabelings. */
  def inputsSubgroup: Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) = pr.nInputsWithOutputRelabelings == 0
    def test(preimage: Int, image: Int) = {
      val preimageIndex = preimage - impShape.offsets(impShape.blockIndices(preimage))
      val imageIndex = image - impShape.offsets(impShape.blockIndices(image))
      preimageIndex == imageIndex
    }
    subgroupFor(test, predicate)
  }

  def outputPermSubgroups: Map[Int, Grp[Perm]] = {
    (0 until nInputs).filter(x => group.generators.exists(r => r.xPerm.movesPoint(x) || !r.aPerm(x).isId)).map { x =>
      x -> outputPermSubgroup(x)
    }.filterNot(_._2.isTrivial).toMap
  }

  def outputPermSubgroup(x: Int): Grp[Perm] =  {
    val prSubGrp = inputSubgroup(x)
    Grp.fromGeneratorsAndOrder(prSubGrp.generators.map(_.aPerm(x)), prSubGrp.order)
  }

  def inputSubgroup(x: Int): Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) =
      (0 until pr.nInputsWithOutputRelabelings).forall(x1 => x1 == x || pr.aPerm(x1).isId)
    def test(preimage: Int, image: Int) = {
      val preimageInput = impShape.blockIndices(preimage)
      val imageInput = impShape.blockIndices(image)
      (preimageInput == x && imageInput == x) || preimage == image
    }
    subgroupFor(test, predicate)
  }

  def outputsSubgroup: Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) = pr.xPerm.isId
    def test(preimage: Int, image: Int) = {
      val preimageInput = impShape.offsets(impShape.blockIndices(preimage))
      val imageInput = impShape.offsets(impShape.blockIndices(image))
      preimageInput == imageInput
    }
    subgroupFor(test, predicate)
  }

}

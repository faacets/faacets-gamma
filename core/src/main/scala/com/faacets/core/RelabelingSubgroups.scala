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

case class RelabelingSubgroups(val group: Grp[Relabeling]) {

  val impRep = ImprimitiveImprimitiveRelabelingRep[Rational](group.generators)

  val impShape = impRep.lattice.shape.imprimitiveImprimitive

  val groupInRep: Grp[Rep.Of[Relabeling, impRep.type]] = Rep.OfGrp(group, impRep)

  def partiesPermSubgroup: Grp[Perm] = {
    val subgrp = partiesSubgroup
    Grp.fromGeneratorsAndOrder(subgrp.generators.map(_.pPerm), subgrp.order)
  }

  def partiesSubgroup: Grp[Relabeling] = {
    def predicate(r: Relabeling) = r.nPartiesWithRelabelings == 0
    def test(preimage: Int, image: Int) = {
      val preimageIndex = preimage - impShape.offsets(impShape.blockIndices(preimage))
      val imageIndex = image - impShape.offsets(impShape.blockIndices(image))
      preimageIndex == imageIndex
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def outputPermSubgroups: Map[(Int, Int), Grp[Perm]] = partyRelabelingSubgroups.flatMap {
    case (p, prGrp) => PartyRelabelingSubgroups(prGrp).outputPermSubgroups.map { case (x, permGrp) => (p, x) -> permGrp }
  }

  def nParties = (1 /: group.generators.map(_.nParties))(_.max(_))

  def partyRelabelingSubgroups: Map[Int, Grp[PartyRelabeling]] =
    (0 until nParties).filter( p => group.generators.exists(r => r.pPerm.movesPoint(p) || !r.partyRelabeling(p).isId) ).map { p =>
      p -> partyRelabelingSubgroup(p)
    }.filterNot(_._2.isTrivial).toMap

  def partyRelabelingSubgroup(p: Int): Grp[PartyRelabeling] = {
    val relSubGrp = partySubgroup(p)
    Grp.fromGeneratorsAndOrder(relSubGrp.generators.map(_.partyRelabeling(p)), relSubGrp.order)
  }

  def inputSubgroup(p: Int, x: Int): Grp[Relabeling] = {
    val prSubGrp = PartyRelabelingSubgroups(partyRelabelingSubgroup(p)).inputSubgroup(x)
    Grp.fromGeneratorsAndOrder(prSubGrp.generators.map(g => Relabeling(Map(p -> g), Group[Perm].id)), prSubGrp.order)
  }

  def partySubgroup(p: Int): Grp[Relabeling] = {
    def predicate(r: Relabeling): Boolean = r.pPerm.isId && {
      cforRange(0 until r.nPartiesWithRelabelings) { p1 =>
        if (p1 != p && (!r.xPerm(p).isId || r.nInputsWithOutputRelabelings(p) > 0)) return false
      }
      true
    }
    def test(preimage: Int, image: Int) = {
      val preimageParty = impShape.blockIndices(preimage)
      val imageParty = impShape.blockIndices(image)
      (preimageParty == p && imageParty == p) || preimage == image
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def inputsOutputsSubgroup: Grp[Relabeling] = {
    def predicate(r: Relabeling) = r.pPerm.isId
    def test(preimage: Int, image: Int) = {
      val preimageParty = impShape.blockIndices(preimage)
      val imageParty = impShape.blockIndices(image)
      preimageParty == imageParty
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def outputsSubgroup: Grp[Relabeling] = {
    val partyShapes = impRep.lattice.shape.partyShapes
    def predicate(r: Relabeling) = r.pPerm.isId && r.nPartiesWithInputRelabelings == 0
    def test(preimage: Int, image: Int) = {
      val preimageParty = impShape.blockIndices(preimage)
      val imageParty = impShape.blockIndices(image)
      preimageParty == imageParty && {
        val preimageIndex = preimage - impShape.offsets(preimageParty)
        val imageIndex = image - impShape.offsets(imageParty)
        val preimageInput = partyShapes(preimageParty).imprimitive.blockIndices(preimageIndex)
        val imageInput = partyShapes(imageParty).imprimitive.blockIndices(imageIndex)
        preimageInput == imageInput
      }
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def niceGenerators: Iterable[Relabeling] = {
    val partiesGrp = partiesSubgroup
    val partiesGen = partiesGrp.generators
    val outputPermSubgrps = outputPermSubgroups
    val liftingsGen = outputPermSubgroups.flatMap {
      case ((p, x), subgrp) => subgrp.generators.map(g => Relabeling(Map(p -> PartyRelabeling(Map(x -> g), Group[Perm].id)), Group[Perm].id))
    }
    val liftingsOrder = (SafeLong(1) /: outputPermSubgrps) { case (cumOrder, (_, subgrp)) => cumOrder * subgrp.order }
    val liftingsGrp = Grp.fromGeneratorsAndOrder(liftingsGen.toIndexedSeq, liftingsOrder)
    val outputsGrp = outputsSubgroup
    val inputsOutputsGrp = inputsOutputsSubgroup
    val outputsAddGen = GrpHelpers.additionalGeneratorsFor(outputsGrp, liftingsGrp, Opt(partiesGrp))
    val inputsOutputsAddGen = GrpHelpers.additionalGeneratorsFor(inputsOutputsGrp, outputsGrp, Opt(partiesGrp))
    val togetherOrder = partiesGrp.order * inputsOutputsGrp.order
    val togetherGrp = Grp.fromGeneratorsAndOrder(partiesGrp.generators ++ inputsOutputsGrp.generators, togetherOrder)
    val restAddGen = GrpHelpers.additionalGeneratorsFor(group, togetherGrp, Opt(partiesGrp))
    partiesGen ++ liftingsGen ++ outputsAddGen ++ inputsOutputsAddGen ++ restAddGen
  }

}
*/
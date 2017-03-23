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

case class PartyRelabelingSubgroups(val group: Grp[PartyRelabeling]) {

  val impRep = ImprimitivePartyRelabelingRep[Rational](group.generators)

  val impShape = impRep.lattice.shape.imprimitive

  val groupInRep: Grp[Rep.Of[PartyRelabeling, impRep.type]] = Rep.OfGrp(group, impRep)

  def inputsPermSubgroup: Grp[Perm] = {
    val subgrp = inputsSubgroup
    Grp.fromGeneratorsAndOrder(subgrp.generators.map(g => (g: PartyRelabeling).xPerm), subgrp.order)
  }

  def inputsSubgroup: Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) = pr.nInputsWithOutputRelabelings == 0
    def test(preimage: Int, image: Int) = {
      val preimageIndex = preimage - impShape.offsets(impShape.blockIndices(preimage))
      val imageIndex = image - impShape.offsets(impShape.blockIndices(image))
      preimageIndex == imageIndex
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def nInputs = (1 /: group.generators.map(_.nInputs))(_.max(_))

  def outputPermSubgroups: Map[Int, Grp[Perm]] =
    (0 until nInputs).filter( x => group.generators.exists(r => r.xPerm.movesPoint(x) || !r.aPerm(x).isId) ).map { x =>
      x -> outputPermSubgroup(x)
    }.filterNot(_._2.isTrivial).toMap

  def outputPermSubgroup(x: Int): Grp[Perm] =  {
    val prSubGrp = inputSubgroup(x)
    Grp.fromGeneratorsAndOrder(prSubGrp.generators.map(_.aPerm(x)), prSubGrp.order)
  }

  def inputSubgroup(x: Int): Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) =
      (0 until pr.nInputsWithOutputRelabelings).forall(x1 => x1 == x || pr.aPerm(x1).isId)
    def test(preimage: Int, image: Int) = {
      val preimageInput = impShape.blockIndices(preimage)
      preimageInput == x || preimage == image
    }
    groupInRep.subgroupFor(test, predicate)
  }

  def outputsSubgroup: Grp[PartyRelabeling] = {
    def predicate(pr: PartyRelabeling) = pr.xPerm.isId
    def test(preimage: Int, image: Int) = {
      val preimageInput = impShape.offsets(impShape.blockIndices(preimage))
      val imageInput = impShape.offsets(impShape.blockIndices(image))
      preimageInput == imageInput
    }
    groupInRep.subgroupFor(test, predicate)
  }

}
*/
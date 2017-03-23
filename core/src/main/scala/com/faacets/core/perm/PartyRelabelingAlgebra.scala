package com.faacets
package core
package perm

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._
import net.alasc.perms.Perm

final class PartyRelabelingEq extends Eq[PartyRelabeling] {

  def eqv(q: PartyRelabeling, r: PartyRelabeling): Boolean = {
    if (q.xPerm =!= r.xPerm) return false
    val n = q.nInputsWithOutputRelabelings.max(r.nInputsWithOutputRelabelings)
    cforRange(0 until n) { x =>
      if (q.aPerm(x) =!= r.aPerm(x)) return false
    }
    true
  }

}

final class PartyRelabelingGroup extends Group[PartyRelabeling] {

  val empty: PartyRelabeling = new PartyRelabelingImpl16(new Array[Long](0), 0L)

  def inverseImplGen(r: PartyRelabeling): PartyRelabeling = {
    val xPermInv = r.xPerm.inverse
    var x = (r.nInputsWithOutputRelabelings - 1).max(r.xPerm.largestMovedPoint.getOrElseFast(-1))
    var newAArray: Array[Perm] = null
    while (x >= 0) {
      val xxpi = x <|+| xPermInv
      val newA = r.aPerm(xxpi).inverse
      if (!newA.isId && (newAArray eq null))
        newAArray = new Array[Perm](x + 1)
      if (newAArray ne null)
        newAArray(x) = newA
      x -= 1
    }
    if (newAArray eq null) newAArray = new Array[Perm](0)
    new PartyRelabelingImplGen(newAArray, xPermInv)
  }

  def inverseImpl16(r16: PartyRelabelingImpl16): PartyRelabeling = {
    import net.alasc.perms.internal.Perm16Encoding.{inverse => inverse16, image => image16, largestMovedPoint => largestMovedPoint16}
    val xPermInv = inverse16(r16.xPermEnc)
    var x = (r16.nInputsWithOutputRelabelings - 1).max(largestMovedPoint16(r16.xPermEnc))
    var newAArray: Array[Long] = null
    while (x >= 0) {
      val xxpi = image16(xPermInv, x)
      val newA = inverse16(r16.aPermEnc(xxpi))
      if (newA != 0 && (newAArray eq null))
        newAArray = new Array[Long](x + 1)
      if (newAArray ne null)
        newAArray(x) = newA
      x -= 1
    }
    if (newAArray eq null) newAArray = new Array[Long](0)
    new PartyRelabelingImpl16(newAArray, xPermInv)
  }

  def inverse(r: PartyRelabeling): PartyRelabeling = r match {
    case r16: PartyRelabelingImpl16 => inverseImpl16(r16)
    case _ => inverseImplGen(r)
  }

  def combineImplGen(q: PartyRelabeling, r: PartyRelabeling): PartyRelabeling = {
    val newXPerm = q.xPerm |+| r.xPerm
    var x = q.nInputsWithOutputRelabelings.max(r.nInputsWithOutputRelabelings).max(q.xPerm.largestMovedPoint.getOrElseFast(-1))
    var newAArray: Array[Perm] = null
    while (x >= 0) {
      val xqx = x <|+| q.xPerm
      val newA = q.aPerm(x) |+| r.aPerm(xqx)
      if (!newA.isId && (newAArray eq null))
        newAArray = new Array[Perm](x + 1)
      if (newAArray ne null)
        newAArray(x) = newA
      x -= 1
    }
    if (newAArray eq null) newAArray = new Array[Perm](0)
    new PartyRelabelingImplGen(newAArray, newXPerm)
  }

  def combineImpl16(q16: PartyRelabelingImpl16, r16: PartyRelabelingImpl16): PartyRelabeling = {
    import net.alasc.perms.internal.Perm16Encoding.{op => op16, image => image16, largestMovedPoint => largestMovedPoint16}
    val newXPermEnc = op16(q16.xPermEnc, r16.xPermEnc)
    var x = q16.nInputsWithOutputRelabelings.max(r16.nInputsWithOutputRelabelings).max(largestMovedPoint16(q16.xPermEnc))
    var newAArray: Array[Long] = null
    while (x >= 0) {
      val xqx = image16(q16.xPermEnc, x)
      val newA = op16(q16.aPermEnc(x), r16.aPermEnc(xqx))
      if (newA != 0L && (newAArray eq null))
        newAArray = new Array[Long](x + 1)
      if (newAArray ne null)
        newAArray(x) = newA
      x -= 1
    }
    if (newAArray eq null) newAArray = new Array[Long](0)
    new PartyRelabelingImpl16(newAArray, newXPermEnc)
  }

  def combine(q: PartyRelabeling, r: PartyRelabeling): PartyRelabeling = q match {
    case q16: PartyRelabelingImpl16 => r match {
      case r16: PartyRelabelingImpl16 => combineImpl16(q16, r16)
      case _ => combineImplGen(q, r)
    }
    case _ => combineImplGen(q, r)
  }

}

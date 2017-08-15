package com.faacets
package core
package perm

import spire.algebra._
import net.alasc.perms._
import net.alasc.syntax.all._

/** Describes the relabeling of inputs and outputs of a party, independently of the size of the
  * party. Outputs for the inputs for which the permutation is not contained in `aArray` are not relabeled.
  */
class PartyRelabelingImpl16 protected[perm] (val aArrayEnc: Array[Long], val xPermEnc: Long) extends PartyRelabeling {
  def nInputsWithOutputRelabelings: Int = aArrayEnc.length
  def aPermEnc(x: Int) = if (x < aArrayEnc.length) aArrayEnc(x) else 0L
  def aPerm(x: Int) = if (x < aArrayEnc.length) new Perm16(aArrayEnc(x)) else Group[Perm].id
  def xPerm = new Perm16(xPermEnc)
  def outputPart: PartyRelabeling = new PartyRelabelingImpl16(aArrayEnc, 0L)
  def inputPart: PartyRelabeling = new PartyRelabelingImpl16(new Array[Long](0), xPermEnc)
}

object PartyRelabelingImpl16 extends PartyRelabelingCompanion {
  def apply(aMap: Map[Int, Perm], xPerm: Perm): PartyRelabeling = {
    var i = (-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).isId => mx
      case (mx, k) => mx.max(k)
    }
    var newAArray: Array[Long] = null
    while (i >= 0) {
      val newA = aMap.getOrElse(i, Group[Perm].id)
      if (!newA.isId && (newAArray eq null))
        newAArray = new Array[Long](i + 1)
      if (newAArray ne null)
        newAArray(i) = newA.asInstanceOf[Perm16].encoding
      i -= 1
    }
    if (newAArray eq null) newAArray = new Array[Long](0)
    new PartyRelabelingImpl16(newAArray, xPerm.asInstanceOf[Perm16].encoding)
  }
}

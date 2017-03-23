package com.faacets
package core
package perm

import spire.algebra._
import net.alasc.perms._
import net.alasc.syntax.all._

/** Describes the relabeling of inputs and outputs of a party, independently of the size of the
  * party. Outputs for the inputs for which the permutation is not contained in `aArray` are not relabeled.
  */
class PartyRelabelingImplGen protected[perm] (val aArray: Array[Perm], val xPerm: Perm) extends PartyRelabeling {
  def nInputsWithOutputRelabelings: Int = aArray.length
  def aPerm(x: Int) = if (x < aArray.length) aArray(x) else Group[Perm].id
  def outputPart: PartyRelabeling = new PartyRelabelingImplGen(aArray, Group[Perm].id)
  def inputPart: PartyRelabeling = new PartyRelabelingImplGen(new Array[Perm](0), xPerm)
}

object PartyRelabelingImplGen extends PartyRelabelingCompanion {
  def apply(aMap: Map[Int, Perm], xPerm: Perm): PartyRelabeling = {
    var i = (-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).isId => mx
      case (mx, k) => mx.max(k)
    }
    var newAArray: Array[Perm] = null
    while (i >= 0) {
      val newA = aMap.getOrElse(i, Group[Perm].id)
      if (!newA.isId && (newAArray eq null))
        newAArray = new Array[Perm](i + 1)
      if (newAArray ne null)
        newAArray(i) = newA
      i -= 1
    }
    if (newAArray eq null) newAArray = new Array[Perm](0)
    new PartyRelabelingImplGen(newAArray, xPerm)
  }
}

package com.faacets
package core
package perm

import spire.algebra._
import spire.syntax.cfor._
import net.alasc.perms._
import net.alasc.syntax.all._

/** Implementation of a Relabeling using an array for output relabelings (concatenated) and an array for input relabelings.
  * 
  * @param nA      Index of the last party with an output relabeling + 1
  * @param aLength Index of the last input with an output relabeling for each party + 1
  * @param aOffset Offset where the output relabelings of each party start in the array
  * @param aArray  Output relabelings for all inputs of all parties, concatenated
  * @param nX      Index of the last party with an input relabeling + 1
  * @param xArray  Input relabeling for the parties `0 ... nX - 1`
  * @param pPerm   Party relabeling
  * 
  * @note The length of `aLength`, `aOffset`, `aArray` can be greater than `nA` because arrays are not resized after some
  *       computation. The extra elements are undefined. Same for `xArray` and `nX`.
  */
class RelabelingImplGen protected[perm] (val nA: Int, val aLength: Array[Int], val aOffset: Array[Int], val aArray: Array[Perm], val nX: Int, val xArray: Array[Perm], val pPerm: Perm) extends Relabeling {
  def aPerm(p: Int, x: Int): Perm =
    if (p >= nA || x >= aLength(p))
      Group[Perm].id
    else
      aArray(aOffset(p) + x)
  def xPerm(p: Int): Perm =
    if (p >= nX)
      Group[Perm].id
    else
      xArray(p)

  def partyPart = new RelabelingImplGen(0, Array.empty[Int], Array.empty[Int], Array.empty[Perm], 0, Array.empty[Perm], pPerm)
  def inputPart = new RelabelingImplGen(0, Array.empty[Int], Array.empty[Int], Array.empty[Perm], nX, xArray, Group[Perm].id)
  def outputPart = new RelabelingImplGen(nA, aLength, aOffset, aArray, 0, Array.empty[Perm], Group[Perm].id)
  def outputInputPart = new RelabelingImplGen(nA, aLength, aOffset, aArray, nX, xArray, Group[Perm].id)

  def nPartiesWithOutputRelabelings: Int = nA
  def nPartiesWithInputRelabelings: Int = nX
  def nPartiesWithInputOutputRelabelings: Int = nA.max(nX)

  def nInputsWithOutputRelabelings(p: Int): Int = if (p >= nA) 0 else aLength(p)

  def partyRelabeling(p: Int): PartyRelabeling = {
    val newAArray = Array.tabulate[Perm](nInputsWithOutputRelabelings(p))( x => aPerm(p, x) )
    val newH = xPerm(p)
    new PartyRelabelingImplGen(newAArray, newH)
  }
}

object RelabelingImplGen extends RelabelingCompanion {
  def fromRelabeling(r: Relabeling): Relabeling = {
    val Relabeling(aMap, pPerm) = r
    apply(aMap, pPerm)
  }
  def apply(aMap: Map[Int, PartyRelabeling], pPerm: Perm): Relabeling = {
    val nA = ((-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).nInputsWithOutputRelabelings == 0 => mx
      case (mx, k) => mx.max(k)
    }) + 1
    val aLength = Array.tabulate(nA)( p => aMap.get(p).map(_.nInputsWithOutputRelabelings).getOrElse(0) )
    val aOffset = aLength.scanLeft(0)(_ + _)
    val aArray = new Array[Perm](aOffset.last)
    var i = 0
    cforRange(0 until nA) { p =>
      cforRange(0 until aLength(p)) { x =>
        aArray(i) = aMap(p).aPerm(x)
        i += 1
      }
    }
    val nX = ((-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).xPerm.isId => mx
      case (mx, k) => mx.max(k)
    }) + 1
    val xArray = Array.tabulate[Perm](nX)( p => aMap.get(p).map(_.xPerm).getOrElse(Group[Perm].id) )
    new RelabelingImplGen(nA, aLength, aOffset, aArray, nX, xArray, pPerm)
  }
}

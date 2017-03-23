package com.faacets
package core
package perm


import scala.reflect.classTag

import spire.algebra._
import spire.syntax.eq._
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.cfor._

import net.alasc.algebra._
import net.alasc.perms._
import net.alasc.syntax.all._
import net.alasc.util._

/** Implementation of a Relabeling using an array for output relabelings (concatenated) and an array for input relabelings.
  * Same as RelabelingImplGen, but with a maximum of 16 outputs, inputs and parties.
  * 
  * Methods/variables postfixed by `enc` return the `Long` encoding of a `Perm16`, those postfixed  by `16` return a
  * `Perm16` value class, the remaining return an instance of the `Perm` universal trait.
  */
class RelabelingImpl16 protected[perm] (val nA: Int, val aLength: Array[Int], val aOffset: Array[Int], val aArrayEnc: Array[Long], val nX: Int, val xArrayEnc: Array[Long], val pPermEnc: Long) extends Relabeling {
  def pPerm16: Perm16 = new Perm16(pPermEnc)
  def pPerm: Perm = pPerm16

  def aPermEnc(p: Int, x: Int): Long =
    if (p >= nA || x >= aLength(p)) 0L else aArrayEnc(aOffset(p) + x)
  def aPerm16(p: Int, x: Int): Perm16 = new Perm16(aPermEnc(p, x))
  def aPerm(p: Int, x: Int): Perm = aPerm16(p, x)

  def xPermEnc(p: Int): Long =
    if (p >= nX) 0L else xArrayEnc(p)
  def xPerm16(p: Int): Perm16 = new Perm16(xPermEnc(p))
  def xPerm(p: Int): Perm = xPerm16(p)

  def partyPart = new RelabelingImpl16(0, Array.empty[Int], Array.empty[Int], Array.empty[Long], 0, Array.empty[Long], pPermEnc)
  def inputPart = new RelabelingImpl16(0, Array.empty[Int], Array.empty[Int], Array.empty[Long], nX, xArrayEnc, 0L)
  def outputPart = new RelabelingImpl16(nA, aLength, aOffset, aArrayEnc, 0, Array.empty[Long], 0L)
  def outputInputPart = new RelabelingImpl16(nA, aLength, aOffset, aArrayEnc, nX, xArrayEnc, 0L)

  def nInputsWithOutputRelabelings(p: Int): Int = if (p >= nA) 0 else aLength(p)
  def nPartiesWithOutputRelabelings: Int = nA
  def nPartiesWithInputRelabelings: Int = nX
  def nPartiesWithRelabelings: Int = nA.max(nX)

  def partyRelabeling(p: Int): PartyRelabeling = {
    val newAArray = Array.tabulate[Long](nInputsWithOutputRelabelings(p))( x => aPermEnc(p, x) )
    val newH = xPermEnc(p)
    new PartyRelabelingImpl16(newAArray, newH)
  }
}

object RelabelingImpl16 extends RelabelingCompanion {
  def fromRelabeling(r: Relabeling): Relabeling = {
    val Relabeling(aMap, pPerm) = r
    apply(aMap, pPerm)
  }
  def apply(aMap: Map[Int, PartyRelabeling], pPerm: Perm): Relabeling = {
    val nPR = (-1 /: aMap.keysIterator)(_.max(_)) + 1
    val nA = ((-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).nInputsWithOutputRelabelings == 0 => mx
      case (mx, k) => mx.max(k)
    }) + 1
    val aLength = Array.tabulate(nA)( p => aMap.get(p).map(_.nInputsWithOutputRelabelings).getOrElse(0) )
    val aOffset = aLength.scanLeft(0)(_ + _)
    val aArray = new Array[Long](aOffset.last)
    var i = 0
    cforRange(0 until nA) { p =>
      cforRange(0 until aLength(p)) { x =>
        aArray(i) = aMap(p).aPerm(x).asInstanceOf[Perm16].encoding
        i += 1
      }
    }
    val nX = ((-1 /: aMap.keysIterator) {
      case (mx, k) if aMap(k).xPerm.isId => mx
      case (mx, k) => mx.max(k)
    }) + 1
    val xArray = Array.tabulate[Long](nX)( p => aMap.get(p).map(_.xPerm.asInstanceOf[Perm16].encoding).getOrElse(0L) )
    new RelabelingImpl16(nA, aLength, aOffset, aArray, nX, xArray, pPerm.asInstanceOf[Perm16].encoding)
  }
}

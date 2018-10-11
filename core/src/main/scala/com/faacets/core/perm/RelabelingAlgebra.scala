package com.faacets
package core
package perm

import com.faacets.core.ref.POVM
import spire.algebra._
import spire.syntax.action._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.group._
import net.alasc.perms.Perm

final class POVMRelabelingAction extends Action[POVM, Relabeling] {

  override def actl(r: Relabeling, povm: POVM): POVM = actr(povm, r.inverse)

  override def actr(povm: POVM, r: Relabeling): POVM = povm match {
    case POVM(p, x, a) =>
      val newA = a <|+| r.aPerm(p, x)
      val newX = x <|+| r.xPerm(p)
      val newP = p <|+| r.pPerm
      POVM(newP, newX, newA)
  }
}

final class RelabelingEq extends Eq[Relabeling] {

  def eqv(q: Relabeling, r: Relabeling): Boolean = {
    if (q.pPerm =!= r.pPerm) return false
    val nParties = q.nPartiesWithInputOutputRelabelings.max(r.nPartiesWithInputOutputRelabelings)
    cforRange(0 until nParties) { p =>
      if (q.xPerm(p) =!= r.xPerm(p))
        return false
      val nInputs = q.nInputsWithOutputRelabelings(p).max(r.nInputsWithOutputRelabelings(p))
      cforRange(0 until nInputs) { x =>
        if (q.aPerm(p, x) =!= r.aPerm(p, x))
          return false
      }
    }
    true
  }
}

final class RelabelingGroup extends Group[Relabeling] {

  val empty: Relabeling = {
    val emptyInt = new Array[Int](0)
    val emptyPerm = new Array[Long](0)
    new RelabelingImpl16(0, emptyInt, emptyInt, emptyPerm, 0, emptyPerm, 0L)
  }

  def combine(f: Relabeling, g: Relabeling): Relabeling =
    f match {
      case f16: RelabelingImpl16 => g match {
        case g16: RelabelingImpl16 => opImpl16(f16, g16)
        case _ => opImplGen(f, g)
      }
      case _ => opImplGen(f, g)
    }

  /** Returns the composition of two relabelings. */
  def opImplGen(f: Relabeling, g: Relabeling): Relabeling = {
    /* Let f = A X P and g = B Y Q, where :
     *
     * - A(p, x), B(p, x) are output relabelings for the input x of party p
     * - X(p), Y(p) are input relabelings for the party p
     * - P, Q are party relabelings.
     */
    @inline def _P = f.pPerm
    @inline def _Q = g.pPerm
    @inline def _X(p: Int) = f.xPerm(p)
    @inline def _Y(p: Int) = g.xPerm(p)
    @inline def _A(p: Int, x: Int) = f.aPerm(p, x)
    @inline def _B(p: Int, x: Int) = g.aPerm(p, x)
    /* We want to compute h = A X P B Y Q = C Z R, where C, Z and R are output/input/party relabelings.
     * Let us look at the action of h = C Z R on some (a,x,p) - a triplet of party/input/output indices:
     * 
     * (a,x,p) <|+| h = (a <|+| C(p,x), x <|+| Z(p), p <|+| Z).
     * 
     * Let us compute the same for h = A X P B Y Q:
     * 
     * (a,x,p) <|+| AXPBYQ = (a <|+| A(p,x), x <|+| X(p), p <|+| P) <|+| BYQ =
     * = (a <|+| A(p,x) B(p <|+| P, x <|+| X(p)), x <|+| X(p) Y(p <|+| P), p <|+| P Q)
     * 
     * and thus:
     * 
     * - C(p,x) = A(p,x) |+| B(p <|+| P, x <|+| X(p))
     * - Z(p) = X(p) |+| Y(p <|+| P)
     * - R = P |+| Q
     */
    val nCTest = f.nPartiesWithOutputRelabelings.max(g.nPartiesWithOutputRelabelings).max(_P.largestMovedPoint.getOrElseFast(-1) + 1)
    val cLength = new Array[Int](nCTest)
    val cOffset = new Array[Int](nCTest)
    var maxCArrayLength = 0
    // for now, fill cLength with the maximal length possible. The real cLength will be computed in a second step.
    cforRange(0 until nCTest) { p =>
      val pP = p <|+| _P
      val cL = f.nInputsWithOutputRelabelings(p).max(g.nInputsWithOutputRelabelings(pP)).max(_X(p).largestMovedPoint.getOrElseFast(-1) + 1)
      cLength(p) = cL
      maxCArrayLength += cL
    }
    // allocate cArray, possibly wasting a bit of space
    val cArray = new Array[Perm](maxCArrayLength)
    // fill now cArray
    var cIndex = 0
    var nC = 0
    cforRange(0 until nCTest) { p =>
      val pP = p <|+| _P
      cOffset(p) = cIndex
      var cLengthTrue = 0
      var x = cLength(p) - 1
      while (x >= 0) {
        val _C = _A(p, x) |+| _B(pP, x <|+| _X(p))
        if (!_C.isId || cLengthTrue != 0) {
          if (cLengthTrue == 0) cLengthTrue = x + 1
          cArray(cIndex + x) = _C
        }
        x -= 1
      }
      if (cLengthTrue > 0) nC = p + 1
      cLength(p) = cLengthTrue
      cIndex += cLengthTrue
    }
    // fill now zArray
    var p = (f.nPartiesWithInputRelabelings.max(g.nPartiesWithInputRelabelings) - 1).max(_P.largestMovedPoint.getOrElseFast(-1))
    var zArray: Array[Perm] = null
    while (p >= 0) {
      val pP = p <|+| _P
      val newZ = _X(p) |+| _Y(pP)
      if (!newZ.isId && (zArray eq null))
        zArray = new Array[Perm](p + 1)
      if (zArray ne null)
        zArray(p) = newZ
      p -= 1
    }
    if (zArray eq null) zArray = new Array[Perm](0)
    val nZ = zArray.length
    val rPerm = _P |+| _Q
    new RelabelingImplGen(nC, cLength, cOffset, cArray, nZ, zArray, rPerm)
  }

  def opImpl16(f: RelabelingImpl16, g: RelabelingImpl16): RelabelingImpl16 = {
    import net.alasc.perms.sized.Perm16Encoding.{image => image16, largestMovedPoint => largestMovedPoint16, op => op16}
    @inline def _Penc = f.pPermEnc
    @inline def _Qenc = g.pPermEnc
    @inline def _Xenc(p: Int) = f.xPermEnc(p)
    @inline def _Yenc(p: Int) = g.xPermEnc(p)
    @inline def _Aenc(p: Int, x: Int) = f.aPermEnc(p, x)
    @inline def _Benc(p: Int, x: Int) = g.aPermEnc(p, x)
    val nCTest = f.nPartiesWithOutputRelabelings.max(g.nPartiesWithOutputRelabelings).max(largestMovedPoint16(_Penc) + 1)
    val cLength = new Array[Int](nCTest)
    val cOffset = new Array[Int](nCTest)
    var maxCArrayLength = 0
    // for now, fill cLength with the maximal length possible. The real cLength will be computed in a second step.
    cforRange(0 until nCTest) { p =>
      val pP = image16(_Penc, p)
      val cL = f.nInputsWithOutputRelabelings(p).max(g.nInputsWithOutputRelabelings(pP)).max(largestMovedPoint16(_Xenc(p)) + 1)
      cLength(p) = cL
      maxCArrayLength += cL
    }
    // allocate cArrayEnc, possibly wasting a bit of space
    val cArrayEnc = new Array[Long](maxCArrayLength)
    // fill now cArrayEnc
    var cIndex = 0
    var nC = 0
    cforRange(0 until nCTest) { p =>
      val pP = image16(_Penc, p)
      cOffset(p) = cIndex
      var cLengthTrue = 0
      var x = cLength(p) - 1
      while (x >= 0) {
        val _Cenc = op16(_Aenc(p, x), _Benc(pP, image16(_Xenc(p), x)))
        if (_Cenc != 0L || cLengthTrue != 0) {
          if (cLengthTrue == 0) cLengthTrue = x + 1
          cArrayEnc(cIndex + x) = _Cenc
        }
        x -= 1
      }
      if (cLengthTrue > 0)
        nC = p + 1
      cLength(p) = cLengthTrue
      cIndex += cLengthTrue
    }
    // fill now zArrayEnc
    var p = (f.nPartiesWithInputRelabelings.max(g.nPartiesWithInputRelabelings) - 1).max(largestMovedPoint16(_Penc))
    var zArrayEnc: Array[Long] = null
    while (p >= 0) {
      val pP = image16(_Penc, p)
      val newZenc = op16(_Xenc(p), _Yenc(pP))
      if (newZenc != 0L && (zArrayEnc eq null))
        zArrayEnc = new Array[Long](p + 1)
      if (zArrayEnc ne null)
        zArrayEnc(p) = newZenc
      p -= 1
    }
    if (zArrayEnc eq null) zArrayEnc = new Array[Long](0)
    val nZ = zArrayEnc.length
    val rPermEnc = op16(_Penc, _Qenc)
    new RelabelingImpl16(nC, cLength, cOffset, cArrayEnc, nZ, zArrayEnc, rPermEnc)
  }

  def inverse(f: Relabeling): Relabeling =
    f match {
      case f16: RelabelingImpl16 => inverseImpl16(f16)
      case _ => inverseGen(f)
    }

  def inverseGen(f: Relabeling): Relabeling = {
    /* We want to compute g = B Y Q such that h = f g = identity. We have (see above) :
     * 
     * - C(p,x) = A(p,x) |+| B(p <|+| P, x <|+| X(p))
     * - B(p <|+| P, x <|+| X(p)) = A(p,x).inverse
     * 
     * - Z(p) = X(p) |+| Y(p <|+| P)
     * - Y(p <|+| P) = X(p).inverse
     * 
     * - R = P |+| Q
     * - Q = P.inverse
     * 
     * and writing q = p <|+| P, y = x <|+| X(q <|+| P.inverse), we have:
     * 
     * - B(q, y) = A(p, x).inverse = A(q <|+| P.inverse, y <|+| X(q <|+| P.inverse).inverse).inverse
     * - Y(q) = X(q <|+| P.inverse).inverse
     * - Q = P.inverse
     */
    @inline def _P = f.pPerm
    @inline def _X(p: Int) = f.xPerm(p)
    @inline def _A(p: Int, x: Int) = f.aPerm(p, x)
    /* Compute in efficient order: Q = P.inverse */
    val _Q = _P.inverse
    /* and Y(q) = X(q <|+| Q).inverse */
    var q = (f.nPartiesWithInputRelabelings - 1).max(_P.largestMovedPoint.getOrElseFast(-1))
    var yArray: Array[Perm] = null
    while (q >= 0) {
      val qQ = q <|+| _Q
      val _Yinverse = _X(qQ)
      if (!_Yinverse.isId && (yArray eq null))
        yArray = new Array[Perm](q + 1)
      if (yArray ne null)
        yArray(q) = _Yinverse.inverse
      q -= 1
    }
    if (yArray eq null) yArray = new Array[Perm](0)
    val nY = yArray.length
    /* and B(q, y) = A(q <|+| Q, y <|+| Y(q)).inverse */
    val nBTest = f.nPartiesWithOutputRelabelings.max(_P.largestMovedPoint.getOrElseFast(-1) + 1)
    val bLength = new Array[Int](nBTest)
    val bOffset = new Array[Int](nBTest)
    var maxBArrayLength = 0
    // for now, fill bLength with the maximal length possible. The real bLength will be computed in a second step.
    cforRange(0 until nBTest) { q =>
      val qQ = q <|+| _Q
      val bL = f.nInputsWithOutputRelabelings(qQ).max(_X(qQ).largestMovedPoint.getOrElseFast(-1) + 1)
      bLength(q) = bL
      maxBArrayLength += bL
    }
    val bArray = new Array[Perm](maxBArrayLength)
    // fill now bArray
    var bIndex = 0
    var nB = 0
    cforRange(0 until nBTest) { q =>
      val qQ = q <|+| _Q
      bOffset(q) = bIndex
      var bLengthTrue = 0
      var y = bLength(q) - 1
      while (y >= 0) {
        val _Binverse = _A(qQ, if (q < yArray.length) (y <|+| yArray(q)) else y)
        if (!_Binverse.isId || bLengthTrue != 0) {
          if (bLengthTrue == 0)
            bLengthTrue = y + 1
          bArray(bIndex + y) = _Binverse.inverse
        }
        y -= 1
      }
      if (bLengthTrue > 0)
        nB = q + 1
      bLength(q) = bLengthTrue
      bIndex += bLengthTrue
    }
    new RelabelingImplGen(nB, bLength, bOffset, bArray, nY, yArray, _Q)
  }
  def inverseImpl16(f: RelabelingImpl16): Relabeling = {
    import net.alasc.perms.sized.Perm16Encoding.{image => image16, inverse => inverse16, largestMovedPoint => largestMovedPoint16}
    /* We want to compute g = B Y Q such that h = f g = identity. We have (see above) :
     * 
     * - C(p,x) = A(p,x) |+| B(p <|+| P, x <|+| X(p))
     * - B(p <|+| P, x <|+| X(p)) = A(p,x).inverse
     * 
     * - Z(p) = X(p) |+| Y(p <|+| P)
     * - Y(p <|+| P) = X(p).inverse
     * 
     * - R = P |+| Q
     * - Q = P.inverse
     * 
     * and writing q = p <|+| P, y = x <|+| X(q <|+| P.inverse), we have:
     * 
     * - B(q, y) = A(p, x).inverse = A(q <|+| P.inverse, y <|+| X(q <|+| P.inverse).inverse).inverse
     * - Y(q) = X(q <|+| P.inverse).inverse
     * - Q = P.inverse
     */
    @inline def _Penc = f.pPermEnc
    @inline def _Xenc(p: Int) = f.xPermEnc(p)
    @inline def _Aenc(p: Int, x: Int) = f.aPermEnc(p, x)
    /* Compute in efficient order: Q = P.inverse */
    val _Qenc = inverse16(_Penc)
    /* and Y(q) = X(q <|+| Q).inverse */
    var q = (f.nPartiesWithInputRelabelings - 1).max(largestMovedPoint16(_Penc))
    var yArrayEnc: Array[Long] = null
    while (q >= 0) {
      val qQ = image16(_Qenc, q)
      val _YinverseEnc = _Xenc(qQ)
      if (_YinverseEnc != 0L && (yArrayEnc eq null))
        yArrayEnc = new Array[Long](q + 1)
      if (yArrayEnc ne null)
        yArrayEnc(q) = inverse16(_YinverseEnc)
      q -= 1
    }
    if (yArrayEnc eq null) yArrayEnc = new Array[Long](0)
    val nY = yArrayEnc.length
    /* and B(q, y) = A(q <|+| Q, y <|+| Y(q)).inverse */
    val nBTest = f.nPartiesWithOutputRelabelings.max(largestMovedPoint16(_Penc) + 1)
    val bLength = new Array[Int](nBTest)
    val bOffset = new Array[Int](nBTest)
    var maxBArrayLength = 0
    // for now, fill bLength with the maximal length possible. The real bLength will be computed in a second step.
    cforRange(0 until nBTest) { q =>
      val qQ = image16(_Qenc, q)
      val bL = f.nInputsWithOutputRelabelings(qQ).max(largestMovedPoint16(_Xenc(qQ)) + 1)
      bLength(q) = bL
      maxBArrayLength += bL
    }
    val bArrayEnc = new Array[Long](maxBArrayLength)
    // fill now bArray
    var bIndex = 0
    var nB = 0
    cforRange(0 until nBTest) { q =>
      val qQ = image16(_Qenc, q)
      bOffset(q) = bIndex
      var bLengthTrue = 0
      var y = bLength(q) - 1
      while (y >= 0) {
        val _BinverseEnc = _Aenc(qQ, if (q < yArrayEnc.length) image16(yArrayEnc(q), y) else y)
        if (_BinverseEnc != 0L || bLengthTrue != 0) {
          if (bLengthTrue == 0)
            bLengthTrue = y + 1
          bArrayEnc(bIndex + y) = inverse16(_BinverseEnc)
        }
        y -= 1
      }
      if (bLengthTrue > 0)
        nB = q + 1
      bLength(q) = bLengthTrue
      bIndex += bLengthTrue
    }
    new RelabelingImpl16(nB, bLength, bOffset, bArrayEnc, nY, yArrayEnc, _Qenc)
  }

}

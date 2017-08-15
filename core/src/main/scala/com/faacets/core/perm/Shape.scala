package com.faacets
package core
package perm

import spire.syntax.action._
import spire.syntax.group._
import net.alasc.algebra._
import net.alasc.util._

final class Shape protected[core] (val parties: Seq[Party]) {

  override def hashCode = parties.hashCode

  override def equals(any: Any) = any match {
    case that: Shape => this.parties == that.parties
    case _ => false
  }

  def n = parties.length

  val partyShapes: Array[PartyShape] = (0 until n).map(parties(_).shape).toArray

  val imprimitiveSizes: Array[Int] = partyShapes.map(_.imprimitive.size)

  lazy val primitiveSizes: Array[Int] = partyShapes.map(_.primitive.size)

  val primitiveImprimitive = PrimitiveShape(imprimitiveSizes)

  val imprimitiveImprimitive = ImprimitiveShape(imprimitiveSizes)

  lazy val primitivePrimitive = PrimitiveShape(primitiveSizes)

  object ImpImpAction extends PermutationAction[Relabeling] {

    def isFaithful: Boolean = true

    def size = imprimitiveImprimitive.size

    def movedPointsUpperBound(r: Relabeling): NNOption = NNSome(size - 1)

    def findMovedPoint(g: Relabeling): NNOption = smallestMovedPoint(g) // TODO: optimize

    def actl(r: Relabeling, k: Int): Int = actr(k, r.inverse)

    def actr16(k: Int, r16: RelabelingImpl16): Int = {
      import net.alasc.perms.internal.Perm16Encoding.{image => image16}
      val block = imprimitiveImprimitive.blockIndices(k).toInt
      val newBlock = image16(r16.pPermEnc, block)
      val sub = k - imprimitiveImprimitive.offsets(block)
      if (block >= r16.nPartiesWithInputOutputRelabelings)
        imprimitiveImprimitive.offsets(newBlock) + sub
      else {
        val imp1 = partyShapes(block).imprimitive // TODO: optimize
        val block1 = imp1.blockIndices(sub).toInt
        val newBlock1 = image16(r16.xPermEnc(block), block1)
        val sub1 = sub - imp1.offsets(block1)
        if (block1 >= r16.nInputsWithOutputRelabelings(block))
          imprimitiveImprimitive.offsets(newBlock) + (imp1.offsets(newBlock1) + sub1)
        else
          imprimitiveImprimitive.offsets(newBlock) + imp1.offsets(newBlock1) + image16(r16.aPermEnc(block, block1), sub1)
      }
    }

    def actrGen(k: Int, r: Relabeling): Int = {
      val block = imprimitiveImprimitive.blockIndices(k).toInt
      val newBlock = block <|+| r.pPerm
      val sub = k - imprimitiveImprimitive.offsets(block)
      if (block >= r.nPartiesWithInputOutputRelabelings)
        imprimitiveImprimitive.offsets(newBlock) + sub
      else {
        val imp1 = partyShapes(block).imprimitive // TODO: optimize
        val block1 = imp1.blockIndices(sub).toInt
        val newBlock1 = block1 <|+| r.xPerm(block)
        val sub1 = sub - imp1.offsets(block1)
        if (block1 >= r.nInputsWithOutputRelabelings(block))
          imprimitiveImprimitive.offsets(newBlock) + (imp1.offsets(newBlock1) + sub1)
        else
          imprimitiveImprimitive.offsets(newBlock) + imp1.offsets(newBlock1) + (sub1 <|+| r.aPerm(block, block1))
      }
    }

    def actr(k: Int, r: Relabeling): Int =
      if (k >= size) k else r match {
        case r16: RelabelingImpl16 => actr16(k, r16)
        case _ => actrGen(k, r)
      }

  }

  object PriImpAction extends PermutationAction[Relabeling] {

    def isFaithful: Boolean = false

    def findMovedPoint(g: Relabeling): NNOption = smallestMovedPoint(g)

    def size = primitiveImprimitive.size

    def movedPointsUpperBound(r: Relabeling): NNOption = NNSome(size - 1)

    def actl(r: Relabeling, k: Int): Int = actr(k, r.inverse)

    def actr16(k: Int, r16: RelabelingImpl16): Int = {
      import net.alasc.perms.internal.Perm16Encoding.{image => image16}
      var rem = k
      var i = 0
      var ind = 0
      val an = r16.nPartiesWithInputOutputRelabelings
      while (i < an) {
        val nextRem = primitiveImprimitive.divide(rem, i)
        val alphai = rem - nextRem * primitiveImprimitive.sizes(i)
        rem = nextRem
        val imp1 = partyShapes(i).imprimitive
        val block1 = imp1.blockIndices(alphai).toInt
        val newBlock1 = image16(r16.xPermEnc(i), block1)
        val sub1 = alphai - imp1.offsets(block1)
        val irp = image16(r16.pPermEnc, i)
        if (block1 >= r16.nInputsWithOutputRelabelings(i))
          ind += primitiveImprimitive.factors(irp) * (imp1.offsets(newBlock1) + sub1)
        else
          ind += primitiveImprimitive.factors(irp) * (imp1.offsets(newBlock1) + image16(r16.aPermEnc(i, block1), sub1))
        i += 1
      }
      while (i < primitiveImprimitive.n) {
        val nextRem = primitiveImprimitive.divide(rem, i)
        val alphai = rem - nextRem * primitiveImprimitive.sizes(i)
        rem = nextRem
        ind += primitiveImprimitive.factors(image16(r16.pPermEnc, i)) * alphai // TODO: simplify this last loop
        i += 1
      }
      ind
    }

    def actrGen(k: Int, r: Relabeling): Int = {
      var rem = k
      var i = 0
      var ind = 0
      val an = r.nPartiesWithInputOutputRelabelings
      while (i < an) {
        val nextRem = primitiveImprimitive.divide(rem, i)
        val alphai = rem - nextRem * primitiveImprimitive.sizes(i)
        rem = nextRem
        val imp1 = partyShapes(i).imprimitive
        val block1 = imp1.blockIndices(alphai).toInt
        val newBlock1 = block1 <|+| r.xPerm(i)
        val sub1 = alphai - imp1.offsets(block1)
        if (block1 >= r.nInputsWithOutputRelabelings(i))
          ind += primitiveImprimitive.factors(i <|+| r.pPerm) * (imp1.offsets(newBlock1) + sub1)
        else
          ind += primitiveImprimitive.factors(i <|+| r.pPerm) * (imp1.offsets(newBlock1) + (sub1 <|+| r.aPerm(i, block1)))
        i += 1
      }
      while (i < primitiveImprimitive.n) {
        val nextRem = primitiveImprimitive.divide(rem, i)
        val alphai = rem - nextRem * primitiveImprimitive.sizes(i)
        rem = nextRem
        ind += primitiveImprimitive.factors(i <|+| r.pPerm) * alphai // TODO: simplify this last loop
        i += 1
      }
      ind
    }

    def actr(k: Int, r: Relabeling): Int =
      if (k >= size) k else r match {
        case r16: RelabelingImpl16 => actr16(k, r16)
        case _ => actrGen(k, r)
      }

  }

  object PriPriAction extends PermutationAction[Relabeling] {

    def isFaithful: Boolean = false

    def findMovedPoint(g: Relabeling): NNOption = smallestMovedPoint(g)

    def size = primitivePrimitive.size

    def movedPointsUpperBound(r: Relabeling): NNOption = NNSome(size - 1)

    def actl(r: Relabeling, k: Int): Int = actr(k, r.inverse)

    def actr16(k: Int, r16: RelabelingImpl16): Int = {
      import net.alasc.perms.internal.Perm16Encoding.{image => image16}
      var rem = k
      var i = 0
      var ind = 0
      val an = r16.nPartiesWithInputOutputRelabelings
      while (i < an) {
        val nextRem = primitivePrimitive.divide(rem, i)
        val alphai = rem - nextRem * primitivePrimitive.sizes(i)
        rem = nextRem
        val pri1 = partyShapes(i).primitive
        var rem1 = alphai
        var i1 = 0
        var ind1 = 0
        val an1 = r16.nInputsWithOutputRelabelings(i)
        while (i1 < an1) {
          val nextRem1 = pri1.divide(rem1, i1)
          val alphai1 = rem1 - nextRem1 * pri1.sizes(i1)
          rem1 = nextRem1
          ind1 += pri1.factors(image16(r16.xPermEnc(i), i1)) * image16(r16.aPermEnc(i, i1), alphai1)
          i1 += 1
        }
        while (i1 < pri1.n) {
          val nextRem1 = pri1.divide(rem1, i1)
          val alphai1 = rem1 - nextRem1 * pri1.sizes(i1)
          rem1 = nextRem1
          ind1 += pri1.factors(image16(r16.xPermEnc(i), i1)) * alphai1
          i1 += 1
        }
        ind += primitivePrimitive.factors(image16(r16.pPermEnc, i)) * ind1
        i += 1
      }
      while (i < primitivePrimitive.n) {
        val nextRem = primitivePrimitive.divide(rem, i)
        val alphai = rem - nextRem * primitivePrimitive.sizes(i)
        rem = nextRem
        ind += primitivePrimitive.factors(image16(r16.pPermEnc, i)) * alphai // TODO: simplify this last loop
        i += 1
      }
      ind
    }

    def actrGen(k: Int, r: Relabeling): Int = {
      var rem = k
      var i = 0
      var ind = 0
      val an = r.nPartiesWithInputOutputRelabelings
      while (i < an) {
        val nextRem = primitivePrimitive.divide(rem, i)
        val alphai = rem - nextRem * primitivePrimitive.sizes(i)
        rem = nextRem
        val pri1 = partyShapes(i).primitive
        var rem1 = alphai
        var i1 = 0
        var ind1 = 0
        val an1 = r.nInputsWithOutputRelabelings(i)
        while (i1 < an1) {
          val nextRem1 = pri1.divide(rem1, i1)
          val alphai1 = rem1 - nextRem1 * pri1.sizes(i1)
          rem1 = nextRem1
          ind1 += pri1.factors(i1 <|+| r.xPerm(i)) * (alphai1 <|+| r.aPerm(i, i1))
          i1 += 1
        }
        while (i1 < pri1.n) {
          val nextRem1 = pri1.divide(rem1, i1)
          val alphai1 = rem1 - nextRem1 * pri1.sizes(i1)
          rem1 = nextRem1
          ind1 += pri1.factors(i1 <|+| r.xPerm(i)) * alphai1
          i1 += 1
        }
        ind += primitivePrimitive.factors(i <|+| r.pPerm) * ind1
        i += 1
      }
      while (i < primitivePrimitive.n) {
        val nextRem = primitivePrimitive.divide(rem, i)
        val alphai = rem - nextRem * primitivePrimitive.sizes(i)
        rem = nextRem
        ind += primitivePrimitive.factors(i <|+| r.pPerm) * alphai // TODO: simplify this last loop
        i += 1
      }
      ind
    }

    def actr(k: Int, r: Relabeling): Int =
      if (k >= size) k else r match {
        case r16: RelabelingImpl16 => actr16(k, r16)
        case _ => actrGen(k, r)
      }

  }

}

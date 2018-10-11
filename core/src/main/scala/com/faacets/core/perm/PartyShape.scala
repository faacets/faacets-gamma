package com.faacets
package core
package perm

import spire.syntax.action._
import spire.syntax.group._
import net.alasc.algebra._
import net.alasc.util._

final class PartyShape protected[core] (val inputs: Seq[Int]) {

  override def hashCode = inputs.hashCode

  override def equals(any: Any) = any match {
    case that: PartyShape => this.inputs == that.inputs
    case _ => false
  }

  def n = inputs.length

  def sizes = inputs

  val imprimitive = ImprimitiveShape(sizes)

  lazy val primitive = PrimitiveShape(sizes)

  object PrimitiveAction extends PermutationAction[PartyRelabeling] {

    def isFaithful: Boolean = false

    def findMovedPoint(g: PartyRelabeling): NNOption = smallestMovedPoint(g)

    def size = primitive.size

    def movedPointsUpperBound(g: PartyRelabeling): NNOption = NNSome(size - 1)

    def actl(pr: PartyRelabeling, k: Int): Int = actr(k, pr.inverse)

    def actr(k: Int, pr: PartyRelabeling): Int =
      if (k >= size) k else {
        var rem = k
        var i = 0
        var ind = 0
        val an = pr.nInputsWithOutputRelabelings
        while (i < an) {
          val nextRem = primitive.divide(rem, i)
          val alphai = rem - nextRem * primitive.sizes(i)
          rem = nextRem
          ind += primitive.factors(i <|+| pr.xPerm) * (alphai <|+| pr.aPerm(i))
          i += 1
        }
        while (i < primitive.n) {
          val nextRem = primitive.divide(rem, i)
          val alphai = rem - nextRem * primitive.sizes(i)
          rem = nextRem
          ind += primitive.factors(i <|+| pr.xPerm) * alphai // TODO: simplify this last loop
          i += 1
        }
        ind
      }

  }

  object ImprimitiveAction extends PermutationAction[PartyRelabeling] {

    def isFaithful: Boolean = true

    override def findMovedPoint(g: PartyRelabeling): NNOption = smallestMovedPoint(g)

    def size = imprimitive.size

    def movedPointsUpperBound(g: PartyRelabeling): NNOption = NNSome(size - 1)

    def actl(pr: PartyRelabeling, k: Int): Int = actr(k, pr.inverse)

    def actr(k: Int, pr: PartyRelabeling): Int = if (k >= size) k else pr match {
      case pr16: PartyRelabelingImpl16 =>
        import net.alasc.perms.sized.Perm16Encoding.{image => image16}
        val block = imprimitive.blockIndices(k).toInt
        val newBlock = image16(pr16.xPermEnc, block)
        val sub = k - imprimitive.offsets(block)
        if (block >= pr.nInputsWithOutputRelabelings)
          imprimitive.offsets(newBlock) + sub
        else
          imprimitive.offsets(newBlock) + image16(pr16.aPermEnc(block), sub)
      case _ =>
        val block = imprimitive.blockIndices(k).toInt
        val newBlock = block <|+| pr.xPerm
        val sub = k - imprimitive.offsets(block)
        if (block >= pr.nInputsWithOutputRelabelings)
          imprimitive.offsets(newBlock) + sub
        else
          imprimitive.offsets(newBlock) + (sub <|+| pr.aPerm(block))
    }

  }

}

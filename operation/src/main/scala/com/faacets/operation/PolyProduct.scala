package com.faacets.operation

import com.faacets.core.{AdditiveGroupoid, Relabeling}
import io.circe.{Encoder, Json}
import net.alasc.domains.Partition
import spire.algebra.partial.PartialAction
import spire.math.Rational
import io.circe.syntax._
import spire.syntax.partialAction._

case class PartitionPolynomial(partition: Partition, coeffs: Map[Set[Int], Rational]) {

  val blockStrings = partition.blocks.map(_.toVector.sorted.map(i => ('A' + i).toChar.toString).mkString)

  override def toString = {
    import com.faacets.core.text.Term.printSeq
    val ct = coeffs.toSeq.collect {
      case (in, coeff) if !coeff.isZero => (coeff, in.toVector.sorted.map(blockStrings(_)).mkString("x"))
    }.sortBy(_._2)
    printSeq(ct)
  }

}

object PartitionPolynomial {

  implicit val encoder: Encoder[PartitionPolynomial] = Encoder.encodeString.contramap(_.toString)

}

case class PolyProduct[V](pp: PartitionPolynomial, extracted: Vector[CanonicalDec[V]]) {
  def allCanonicals: Seq[V] = extracted.map(_.canonical)
  def original(implicit G: AdditiveGroupoid[V], A: PartialAction[V, Affine], L: PartialAction[V, Lifting], O: PartialAction[V, Reordering], R: PartialAction[V, Relabeling], P: Tensor[V]): V = {
    import pp.{coeffs, partition}
    val n = partition.nBlocks
    require(extracted.length == n)
    val originals = extracted.map(_.original)
    val zeros = originals.map(v => G.groupoid.leftId(v))
    val ones = zeros.map(v => (v <|+|? Affine(1, 1)).get)
    val zero = P(partition, zeros)
    coeffs.foldLeft(zero) {
      case (acc, (in, coeff)) =>
        val exprs = Vector(0 until n: _*).map { b => if (in.contains(b)) originals(b) else ones(b) }
        val expr = (P(partition, exprs) <|+|? Affine(coeff, 0)).get
        G.groupoid.partialOp(acc, expr).get
    }
  }
}

object PolyProduct {

  implicit def encoder[V:Encoder]: Encoder[PolyProduct[V]] = Encoder.instance { polyProd =>
    val polyJson = "poly" -> polyProd.pp.asJson
    val componentsJson = (polyProd.pp.blockStrings zip polyProd.extracted.map(_.asJson))
    Json.obj(polyJson +: componentsJson: _*)
  }

  // should left be < right ??? TODO
  def merge2[V](partition: Partition, left: PolyProduct[V], right: PolyProduct[V], shift: Rational = Rational.zero): PolyProduct[V] = {

    // we work with these intermediate types
    type FinalBlock = Set[Int]
    type Extracteds = Map[FinalBlock, CanonicalDec[V]]
    type FinalPartition = Set[FinalBlock]
    type Coeffs = Map[Set[FinalBlock], Rational]

    require(partition.nBlocks == 2)
    require(partition.size == left.pp.partition.size + right.pp.partition.size)

    // here is the translation of block indices
    val leftFinalBlock: Vector[Int] = partition.blocks(0).toVector.sorted
    val rightFinalBlock: Vector[Int] = partition.blocks(1).toVector.sorted

    val leftExtracteds: Extracteds = (left.pp.partition.blocks.map(block => block.map(leftFinalBlock(_))).toVector zip left.extracted).toMap
    val rightExtracteds: Extracteds = (right.pp.partition.blocks.map(block => block.map(rightFinalBlock(_))).toVector zip right.extracted).toMap
    val leftBlocks: FinalPartition = left.pp.partition.blocks.map(block => block.map(leftFinalBlock(_))).toSet
    val rightBlocks: FinalPartition = right.pp.partition.blocks.map(block => block.map(rightFinalBlock(_))).toSet
    val leftCoeffs: Coeffs = left.pp.coeffs.map { case (blockInds, r) => (blockInds.map(b => left.pp.partition.blocks(b).map(leftFinalBlock(_))), r) }
    val rightCoeffs: Coeffs = right.pp.coeffs.map { case (blockInds, r) => (blockInds.map(b => right.pp.partition.blocks(b).map(rightFinalBlock(_))), r) }

    val finalPartition = Partition((leftBlocks ++ rightBlocks).toSeq: _*)
    val finalBlocksMap = finalPartition.blocks.map(_.toSet).zipWithIndex.toMap
    // LMN x RQS
    val lrCoeffs: Map[Set[Int], Rational] = (for {
      (leftSet, leftR) <- leftCoeffs
      (rightSet, rightR) <- rightCoeffs
    } yield (leftSet.map(finalBlocksMap(_)) ++ rightSet.map(finalBlocksMap(_)), leftR * rightR))
    val finalCoeffs = lrCoeffs.updated(Set.empty[Int], lrCoeffs.getOrElse(Set.empty[Int], Rational.zero) + shift)
    val allExtracteds: Extracteds = leftExtracteds ++ rightExtracteds
    val finalExtracteds = finalPartition.blocks.toVector.map(block => allExtracteds(block.toSet))
    PolyProduct(PartitionPolynomial(finalPartition, finalCoeffs), finalExtracteds)
  }

  def ofSingle[V](v: V)(implicit cwae: CanonicalWithAffineExtractor[V]): PolyProduct[V] = ofSingle(cwae(v))

  def ofSingle[V](cwa: CanonicalDecWithAffine[V]): PolyProduct[V] = {
    val partition = Partition(Set(0 until cwa.originalScenario.nParties: _*))
    val (Affine(m, s), c) = cwa.withoutAffine
    val poly = Map(Set.empty[Int] -> s, Set(0) -> m)
    PolyProduct(PartitionPolynomial(partition, poly), Vector(c))
  }
}
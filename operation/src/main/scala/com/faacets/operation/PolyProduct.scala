package com.faacets.operation

import com.faacets.core.{AdditiveGroupoid, Expr, Relabeling}
import com.faacets.operation.product.{PartitionHelpers, Rank1}
import io.circe.{Encoder, Json}
import net.alasc.domains.{Partition, PartitionMap}
import spire.algebra.partial.PartialAction
import spire.math.Rational
import io.circe.syntax._
import scalin.immutable.Mat
import spire.algebra.Action
import spire.syntax.action._
import spire.syntax.partialAction._
import spire.syntax.group._
import spire.util.Opt

import scala.collection.immutable.BitSet

/** Represents a sum of tensor products of expressions of type A. */
case class PolyProduct[A](components: Map[Set[Int], A], coeffs: Map[Set[Set[Int]], Rational]) {

  override def toString = polyString + ";" + components.toString

  require(coeffs.values.forall(!_.isZero))

  def +(r: Rational): PolyProduct[A] = {
    val newCoeffs = coeffs.updated(Set.empty[Set[Int]], coeffs.getOrElse(Set.empty[Set[Int]], Rational.zero) + r).filterNot(_._2.isZero)
    PolyProduct(components, newCoeffs)
  }

  def polyString: String = {
    import com.faacets.core.text.Term.printSeq
    val ct = coeffs.toSeq.map {
      case (in, coeff) => (coeff, PolyProduct.partsString(in))
    }.sortBy(_._2)
    printSeq(ct)
  }

  def original(implicit G: AdditiveGroupoid[A], A: Action[A, Affine], T: Tensor[A]): A = {
    val zeros = components.mapValues(G.groupoid.leftId)
    val ones = zeros.mapValues(_ <|+| Affine(Rational.one, Rational.one))
    val allParts = components.keySet
    val zero = Tensor[A].apply(allParts.map( part => (part -> zeros(part)) ).toMap)
    coeffs.foldLeft(zero) {
      case (acc, (partsPresent, coeff)) =>
        val componentsForCoeff: Map[Set[Int], A] = allParts.map { part =>
          val component: A = if (partsPresent.contains(part)) components(part) else ones(part)
          (part -> component)
        }.toMap
        val newTerm = Tensor[A].apply(componentsForCoeff) <|+| Affine(coeff, Rational.zero)
        G.groupoid.partialOp(acc, newTerm).get
    }
  }

  def map[B](f: A => B): PolyProduct[B] = PolyProduct(components.mapValues(f), coeffs)

  def toProductTreeOption: Option[ProductTree[A]] =
    if (components.size == 1) {
      val Seq((allSet, a)) = components.toSeq
      val mult = coeffs.getOrElse(Set(allSet), Rational.zero)
      val shift = coeffs.getOrElse(Set.empty[Set[Int]], Rational.zero)
      Some(ProductTree.Leaf(a, allSet.size, Affine(mult, shift)))
    } else
      SetPartition.nonTrivialBipartitions(components.keySet).toStream.flatMap(bip => trySplit(bip)).headOption


  def trySplit(bipartition: SetPartition[Set[Int]]): Option[ProductTree[A]] = {
    val Seq(row: Set[Set[Int]], col: Set[Set[Int]]) = bipartition.parts.toSeq
    val rowAllO: Seq[Int] = row.flatten.toSeq.sorted
    val colAllO: Seq[Int] = col.flatten.toSeq.sorted
    val rowO = row.toSeq
    val colO = col.toSeq
    val rowIndices: Seq[Set[Set[Int]]] = (0 until (1 << row.size)).map(i => BitSet.fromBitMask(Array(i)).map(i => rowO(i)))
    val colIndices: Seq[Set[Set[Int]]] = (0 until (1 << col.size)).map(i => BitSet.fromBitMask(Array(i)).map(i => colO(i)))
    import scalin.immutable.dense._
    val matrix = Mat.tabulate[Rational](rowIndices.size, colIndices.size) { (r, c) =>
      val rowSet = rowIndices(r)
      val colSet = colIndices(c)
      coeffs.getOrElse(rowSet ++ colSet, Rational.zero)
    }
    Rank1.decompositionWithShift(matrix).flatMap {
      case (shift, (c, r)) =>
        val rowTranslation: Map[Int, Int] = rowAllO.zipWithIndex.toMap
        val colTranslation: Map[Int, Int] = colAllO.zipWithIndex.toMap
        val rowCoeffs: Map[Set[Set[Int]], Rational] =
          (rowIndices.map(_.map(_.map(rowTranslation))) zip r.toIndexedSeq).filterNot(_._2.isZero).toMap
        val colCoeffs: Map[Set[Set[Int]], Rational] =
          (colIndices.map(_.map(_.map(colTranslation))) zip c.toIndexedSeq).filterNot(_._2.isZero).toMap
        val rowComponents: Map[Set[Int], A] = row.map(b => b.map(rowTranslation) -> components(b)).toMap
        val colComponents: Map[Set[Int], A] = col.map(b => b.map(colTranslation) -> components(b)).toMap
        val rowPolyProduct = PolyProduct(rowComponents, rowCoeffs)
        val colPolyProduct = PolyProduct(colComponents, colCoeffs)
        for {
          rpt <- rowPolyProduct.toProductTreeOption
          cpt <- colPolyProduct.toProductTreeOption
        } yield ProductTree.Node(Map(row.flatten -> rpt, col.flatten -> cpt), Affine(Rational.one, shift))
    }
  }

}

object PolyProduct {

  def partString(set: Set[Int]) = set.toVector.sorted.map(i => ('A' + i).toChar.toString).mkString

  def partsString(set: Set[Set[Int]]) = set.toVector.sortBy(_.min).map(partString).mkString("x")

  implicit def affineAction[A]: Action[PolyProduct[A], Affine] = new Action[PolyProduct[A], Affine] {
    def actr(pp: PolyProduct[A], a: Affine): PolyProduct[A] = {
      val ct = pp.coeffs.getOrElse(Set.empty[Set[Int]], Rational.zero)
      val rest = pp.coeffs.filterKeys(_.nonEmpty)
      val coeffs = rest.mapValues(_ * a.multiplier) ++ Some(ct).filterNot(_.isZero).map(Set.empty[Set[Int]] -> _)
      PolyProduct(pp.components, coeffs)
    }
    def actl(a: Affine, p: PolyProduct[A]): PolyProduct[A] = actr(p, a.inverse)
  }

  implicit def encoder[A:Encoder]: Encoder[PolyProduct[A]] = Encoder.instance { polyProd =>
    val polyJson = "poly" -> Json.fromString(polyProd.polyString)
    val componentsJson = polyProd.components.toSeq.map { case (part, a) => (partString(part) -> a.asJson) }.sortBy(_._1)
    Json.obj(polyJson +: componentsJson: _*)
  }

  implicit def tensor[A]: Tensor[PolyProduct[A]] = new Tensor[PolyProduct[A]] {
    def apply(components: Map[Set[Int], PolyProduct[A]]): PolyProduct[A] = {
      // translation from 0..n-1 indices to final indices for each part
      val translation: Map[Set[Int], Map[Int, Int]] = components.keySet.map { set =>
        set -> set.toSeq.sorted.zipWithIndex.map(_.swap).toMap
      }.toMap
      def tensorCoeffs(current: Map[Set[Set[Int]], Rational], add: Map[Set[Set[Int]], Rational]): Map[Set[Set[Int]], Rational] =
        for {
          (sets, coeff1) <- current
          (set, coeff2) <- add
        } yield ((sets ++ set) -> coeff1 * coeff2)
      val translatedCoeffs: Seq[Map[Set[Set[Int]], Rational]] = components.toSeq.map {
        case (part, PolyProduct(_, coeffs)) => coeffs.map { case (k,v) => k.map(_.map(translation(part).apply(_))) -> v }
      }
      val newCoeffs = translatedCoeffs.tail.foldLeft(translatedCoeffs.head)(tensorCoeffs)
      val newComponents: Map[Set[Int], A] = components.flatMap {
        case (part, PolyProduct(partComponents, _)) => partComponents.map { case (k,v) => k.map(translation(part).apply(_)) -> v }
      }
      PolyProduct(newComponents, newCoeffs)
    }
  }

  def ofSingle[A](a: A)(implicit cwae: CanonicalWithAffineExtractor[A]): PolyProduct[CanonicalDec[A]] = {
    val cwa = cwae(a)
    val allSet = Set(0 until cwa.originalScenario.nParties: _*)
    val (Affine(m, s), c) = cwa.withoutAffine
    val coeffs = Map(Set.empty[Set[Int]] -> s, Set(allSet) -> m).filterNot(_._2.isZero)
    PolyProduct(Map(allSet -> c), coeffs)
  }

}


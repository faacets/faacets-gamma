package com.faacets.operation

import com.faacets.core.{AdditiveGroupoid, Expr, Relabeling}
import com.faacets.data.Textable
import com.faacets.operation.product.Rank1
import io.circe._
import spire.math.Rational
import io.circe.syntax._
import scalin.immutable.Mat
import spire.algebra.Action
import spire.syntax.action._
import cats.instances.all._
import com.faacets.data.instances.all._
import cats.syntax.all._

import scala.collection.immutable.BitSet

case class PolyExpr(coeffs: Map[Set[Set[Int]], Rational]) {

  override def toString: String = {
    import com.faacets.core.text.Term.printSeq
    val ct = coeffs.toSeq.map {
      case (in, coeff) => (coeff, PolyProduct.partsString(in))
    }.sortBy(_._2)
    printSeq(ct)
  }

}

object PolyExpr {

  implicit val textable: Textable[PolyExpr] = Textable.fromParser(Parsers.polyExpr, _.toString)

}

/** Represents a sum of tensor products of expressions of type A. */
case class PolyProduct[A](components: Map[Set[Int], A], coeffs: Map[Set[Set[Int]], Rational]) {

  override def toString = PolyExpr(coeffs).toString + ";" + components.toString

  require(coeffs.values.forall(!_.isZero))

  def +(r: Rational): PolyProduct[A] = {
    val newCoeffs = coeffs.updated(Set.empty[Set[Int]], coeffs.getOrElse(Set.empty[Set[Int]], Rational.zero) + r).filterNot(_._2.isZero)
    PolyProduct(components, newCoeffs)
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

  /** Extracts a possible affine transform from the elements A.
    *
    * @param f Function such that f(a) = (f, b) and "a = b <|+| f" (in spirit)
    */
  def mapAffine[B](f: A => (Affine, B)): PolyProduct[B] = {
    val trans: Map[Set[Int], (Affine, B)] = components.mapValues(f)
    val affines: Map[Set[Int], Affine] = trans.mapValues(_._1)
    val newComp: Map[Set[Int], B] = trans.mapValues(_._2)
    type CoeffSeq = Seq[(Set[Set[Int]], Rational)]
    val startCoeff: CoeffSeq = Seq(Set.empty[Set[Int]] -> Rational.one)
    // for each original coefficient
    val newCoeffsSeq = coeffs.toSeq.flatMap {
      // we do the development of (a*x + b) * (c*y + d) ...
      case (parts, coeff) => parts.foldLeft(startCoeff) { (seq, newPart) =>
        seq.flatMap { case (part1, r) =>
          val affine = affines(newPart)
          val shiftRes = part1 -> (r * affine.shift)
          val multRes = (part1 + newPart) -> (r * affine.multiplier)
          Seq(shiftRes, multRes)
        }
      }
    }
    val newCoeffs = newCoeffsSeq.groupBy(_._1).mapValues { seq =>
      seq.map(_._2).fold(Rational.zero)(_ + _)
    }
    PolyProduct(newComp, newCoeffs)
  }

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
      case (shift, (r, c)) =>
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
    val polyJson = "poly" -> PolyExpr(polyProd.coeffs).asJson
    val componentsJson = polyProd.components.toSeq.map { case (part, a) => (partString(part) -> a.asJson) }.sortBy(_._1)
    Json.obj(polyJson +: componentsJson: _*)
  }

  implicit def decoder[A:Decoder]: Decoder[PolyProduct[A]] = new Decoder[PolyProduct[A]] {

    def apply(c: HCursor): Decoder.Result[PolyProduct[A]] = decodeAccumulating(c).leftMap(_.head).toEither

    override def decodeAccumulating(c: HCursor): AccumulatingDecoder.Result[PolyProduct[A]] =
      Decoder[PolyExpr].tryDecodeAccumulating(c.downField("poly")) andThen { poly =>
        val partSets = poly.coeffs.keySet.flatten
        val parts: Vector[Set[Int]] = partSets.toVector
        val res: Vector[AccumulatingDecoder.Result[(Set[Int], A)]] =
          parts.map( b => Decoder[A].tryDecodeAccumulating(c.downField(partString(b))).map( r => (b -> r) ) )
        val res1: AccumulatingDecoder.Result[Vector[(Set[Int], A)]] = res.sequenceU
        res1.map( components => PolyProduct(components.toMap, poly.coeffs) )
      }

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

  def ofSingle[A](affine: Affine, a: A, n: Int): PolyProduct[A] = {
    val allSet = Set(0 until n: _*)
    val shiftSet = Set.empty[Set[Int]]
    val multSet = Set(allSet)
    val coeffs = Map(shiftSet -> affine.shift, multSet -> affine.multiplier).filterNot(_._2.isZero)
    PolyProduct(Map(allSet -> a), coeffs)
  }

}

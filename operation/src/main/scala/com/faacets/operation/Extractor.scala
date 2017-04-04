package com.faacets
package operation

import com.faacets.core.Relabeling
import io.circe.{Encoder, Json}
import net.alasc.domains.Partition
import spire.algebra.{Action, Eq, Group, Semigroup}
import spire.algebra.partial.{Groupoid, PartialAction}
import spire.math.Rational
import spire.util.Opt
import spire.syntax.partialAction._
import spire.syntax.groupoid._
import syntax.extractor._
import io.circe.syntax._
import com.faacets.data.instances.textable._

trait Extractor[E] {

  def canExtract(e: E): Boolean

}

trait Extracted[E] {

  def original: E

}

object Extracted {

  case class Product[V](partition: Partition, polynomial: Map[Set[Int], Rational], extracted: IndexedSeq[Canonical[V]])

  case class Operation[V, O](val original: V, val operation: O) {
    def extracted(implicit pa: PartialAction[V, O]): V = pa.partialActr(original, operation).get
    /** Returns a pair (v, op) such that v is nondegenerate and v <|+| op is the original element. */
    def extractedPair(implicit pa: PartialAction[V, O], g: Groupoid[O]): (V, O) = (extracted, operation.inverse)
  }

  case class Canonical[V](lifting: Lifting, reordering: Reordering, relabeling: Relabeling, canonical: V)
                         (implicit L: PartialAction[V, Lifting],
                          O: PartialAction[V, Reordering],
                          R: PartialAction[V, Relabeling]) {
    def original: V = {
      val step1 = (canonical <|+|? relabeling).get
      val step2 = (step1 <|+|? reordering).get
      val step3 = (step2 <|+|? lifting).get
      step3
    }
  }

  object Canonical {

    implicit def encoder[V:Encoder]: Encoder[Canonical[V]] = new Encoder[Canonical[V]] {
      def apply(ld: Canonical[V]): Json = {
        val fields = Seq(
          someIfNotId(ld.lifting).map(l => "lifting" -> l.asJson),
          someIfNotId(ld.reordering).map(o => "reordering" -> o.asJson),
          someIfNotId(ld.relabeling).map(r => "relabeling" -> r.asJson)
        ).flatten :+ ("canonical" -> ld.canonical.asJson)
        Json.obj(fields: _*)
      }
    }

  }

  case class CanonicalWithAffine[V](affine: Affine,
                                    lifting: Lifting,
                                    reordering: Reordering,
                                    relabeling: Relabeling,
                                    canonical: V)
                                   (implicit A: PartialAction[V, Affine],
                                    L: PartialAction[V, Lifting],
                                    O: PartialAction[V, Reordering],
                                    R: PartialAction[V, Relabeling]) {
    def original: V = {
      val step1 = (canonical <|+|? relabeling).get
      val step2 = (step1 <|+|? reordering).get
      val step3 = (step2 <|+|? lifting).get
      val step4 = (step3 <|+|? affine).get
      step4
    }
  }

  def someIfNotId[O:Eq:Groupoid](op: O): Option[O] = if (Groupoid[O].isId(op)) None else Some(op)

  object CanonicalWithAffine {
    def apply[V](original: V)(implicit
                              A: PartialAction[V, Affine], AE: OperationExtractor[V, Affine],
                              L: PartialAction[V, Lifting], LE: OperationExtractor[V, Lifting],
                              O: PartialAction[V, Reordering], OE: OperationExtractor[V, Reordering],
                              R: PartialAction[V, Relabeling], RE: OperationExtractor[V, Relabeling])
    : CanonicalWithAffine[V] = {
      val (res1, l) = original.forceExtract[Lifting].extractedPair
      val (res2, o) = res1.forceExtract[Reordering].extractedPair
      val (res3, a) = res2.forceExtract[Affine].extractedPair
      val (res4, r) = res3.forceExtract[Relabeling].extractedPair
      CanonicalWithAffine(a, l, o, r, res4)
    }

    implicit def encoder[V:Encoder]: Encoder[CanonicalWithAffine[V]] = new Encoder[CanonicalWithAffine[V]] {
      def apply(ld: CanonicalWithAffine[V]): Json = {
        val fields = Seq(
          someIfNotId(ld.affine).map(a => "affine" -> a.asJson),
          someIfNotId(ld.lifting).map(l => "lifting" -> l.asJson),
          someIfNotId(ld.reordering).map(o => "reordering" -> o.asJson),
          someIfNotId(ld.relabeling).map(r => "relabeling" -> r.asJson)
        ).flatten :+ ("canonical" -> ld.canonical.asJson)
        Json.obj(fields: _*)
      }
    }

  }



}

/*
trait ProductExtractor[E] {

  def partialExtract(e: E): Opt[Extracted.Product[E]]

  def forceExtract(e: E): Extracted.Product[E] = partialExtract(e) getOrElse {

  }

}*/


/*
object ProductExtractor {
  def allBipartitions(n: Int): IndexedSeq[Domain#Partition] = new IndexedSeq[Domain#Partition] {
    val bitset = scala.collection.immutable.BitSet(0 until n: _*)
    def length = ((1 << n) - 2)/2 // 2^n possibilities - 2 (we remove the two cases with an empty block)
    def apply(index: Int) = {
      val bits = index + 1 // the integer 0 is a bit vector representing a partition with an empty block
      val (block0, block1) = bitset.partition(b => (bits & (1 << b)) == 0)
      Domain.Partition(block0, block1)
    }
  }
}
*/


trait OperationExtractor[E, O] extends Extractor[E] { self =>

  implicit def partialAction: PartialAction[E, O]

  implicit def groupoid: Groupoid[O]

  def identity(e: E): O

  def canExtract(e: E): Boolean = partialExtract(e).nonEmpty

  /** If the given element `e` is not reduced, finds an operation `o` such that the reduced
    * value is given by `u = e <|+| o` and returns Opt(o) or Opt.empty[O]
    */
  def extractOperation(e: E): Opt[O]

  def partialExtract(e: E): Opt[Extracted.Operation[E, O]] = extractOperation(e) match {
    case Opt(o) => Opt(Extracted.Operation(e, o))
    case _ => Opt.empty[Extracted.Operation[E, O]]
  }

  def forceExtract(e: E): Extracted.Operation[E, O] = partialExtract(e).getOrElse(Extracted.Operation(e, identity(e)))

}

object OperationExtractor {

  def apply[E, O](implicit ev: OperationExtractor[E, O]): OperationExtractor[E, O] = ev

}

trait GroupOperationExtractor[E, O] extends OperationExtractor[E, O] { self =>

  implicit def group: Group[O]

  def identity(e: E): O = group.empty

  def groupoid = new Groupoid[O] {

    def inverse(o: O): O = group.inverse(o)

    def partialOp(x: O, y: O): Opt[O] = Opt(group.combine(x, y))

  }

}

trait GroupActionOperationExtractor[E, O] extends GroupOperationExtractor[E, O] { self =>

  implicit def action: Action[E, O]

  def partialAction = new PartialAction[E, O] {

    def partialActr(p: E, g: O): Opt[E] = Opt(action.actr(p, g))

    def partialActl(g: O, p: E): Opt[E] = Opt(action.actl(g, p))

  }

}



/*
trait Decomposition[A] {

    /*        val parties1: Seq[Party] = (expr.scenario.parties zip in1).filter(_._2).map(_._1)
     val parties2: Seq[Party] = (expr.scenario.parties zip in1).filterNot(_._2).map(_._1)
     val scenario1 = Scenario(parties1)
     val scenario2 = Scenario(parties2)
     val r = exprToDecompose.representation
     val rExpr1 = Expr(scenario1, cRepresentation, coeffs1).to(r)
     val rExpr2 = Expr(scenario2, cRepresentation, coeffs2).to(r)
     val Affine(factor1, _) = Affine.ExprExtractor.forceExtract(rExpr1)
     val Affine(factor2, _) = Affine.ExprExtractor.forceExtract(rExpr2)
     val rExpr1Norm = {
     import rExpr1.scenario.ExprVectorSpace
     rExpr1 :/ factor1
     }
     val rExpr2Norm = {
     import rExpr2.scenario.ExprVectorSpace
     rExpr2 :/ factor2
     }
     return Some((Affine(factor1 * factor2, shift), in1, rExpr1Norm, rExpr2Norm))*/
}
*/
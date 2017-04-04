package com.faacets
package operation

import com.faacets.core.Relabeling
import io.circe.{Decoder, Encoder, Json}
import spire.algebra.partial.PartialAction
import spire.syntax.partialAction._
import spire.syntax.group._
import spire.syntax.groupoid._
import syntax.extractor._
import net.alasc.syntax.group._
import io.circe.syntax._
import data.instances.all._

case class LinearDecomposition[V](affine: Affine,
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

object LinearDecomposition {

  def apply[V](original: V)(implicit
                            A: PartialAction[V, Affine], AE: OperationExtractor[V, Affine],
                            L: PartialAction[V, Lifting], LE: OperationExtractor[V, Lifting],
                            O: PartialAction[V, Reordering], OE: OperationExtractor[V, Reordering],
                            R: PartialAction[V, Relabeling], RE: OperationExtractor[V, Relabeling])
  : LinearDecomposition[V] = {
    val (l, res1) = original.extracted[Lifting]
    val (o, res2) = res1.extracted[Reordering]
    val (a, res3) = res2.extracted[Affine]
    val (r, res4) = res3.extracted[Relabeling]
    LinearDecomposition(a.inverse, l.inverse, o.inverse, r.inverse, res4)
  }

  implicit def encoder[V:Encoder]: Encoder[LinearDecomposition[V]] = new Encoder[LinearDecomposition[V]] {

    def apply(ld: LinearDecomposition[V]): Json = {
      val fields = Seq(
        if (ld.affine.isId) None else Some("affine" -> ld.affine.asJson),
        if (ld.lifting.isId) None else Some("lifting" -> ld.lifting.asJson),
        if (ld.reordering.isId) None else Some("reordering" -> ld.reordering.asJson),
        if (ld.relabeling.isId) None else Some("relabeling" -> ld.relabeling.asJson)
      ).flatten :+ ("canonical" -> ld.canonical.asJson)
      Json.obj(fields: _*)
    }

  }

  /*implicit def decoder[V:Decoder]: Decoder[LinearDecomposition[V]] = new Decoder[LinearDecomposition[V]] {



  }*/

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
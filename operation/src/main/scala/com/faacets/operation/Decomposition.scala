package com.faacets
package operation
/*
import scala.reflect.ClassTag

import org.kiama.rewriting.Rewritable

import spire.algebra.{PartialAction, Semigroup}
import spire.syntax.semigroup._
import spire.syntax.action._

import core.perm.Relabeling

import scala.collection.immutable.{Seq => ISeq}

trait Decomposition[A] {
  implicit def classTag: ClassTag[A]

  implicit def productExtractor: ProductExtractor[A]
  implicit def semigroup: Semigroup[A] = productExtractor.semigroup

  implicit def affineExtractor: OperationExtractor[A, Affine]
  implicit def affineAction: PartialAction[A, Affine] = affineExtractor.action

  implicit def reorderingExtractor: OperationExtractor[A, Reordering]
  implicit def reorderingAction: PartialAction[A, Reordering] = reorderingExtractor.action

  implicit def relabelingExtractor: OperationExtractor[A, Relabeling]
  implicit def relabelingAction: PartialAction[A, Relabeling] = relabelingExtractor.action

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
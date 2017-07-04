package com.faacets.operation.product

import com.faacets.core.Expr
import com.faacets.data.Value
import com.faacets.operation.{BellExpression, LowerOrientation, Tensor, UpperOrientation}

import scala.collection.immutable.ListMap

final class BellExpressionTensor extends Tensor[BellExpression] {

  def boundsProduct(components: Iterable[(Value, Value)]): Option[(Value, Value)] = {
    None
  }

  def apply(components: Map[Set[Int], BellExpression]): BellExpression = {
    val exprs = components.mapValues(_.expr)
    val newExpr: Expr = Tensor[Expr].apply(exprs)
    if (components.size == 1) {
      val be = components.head._2
      BellExpression(newExpr,
        be.lower.filterBoundsAndFacetOf(BellExpression.stdPreserved),
        be.upper.filterBoundsAndFacetOf(BellExpression.stdPreserved)
      )
    } else {
      val lbs = components.map(_._2.lower.bounds.keySet).reduce((x: Set[String], y: Set[String]) => x intersect y)
      val ubs = components.map(_._2.upper.bounds.keySet).reduce((x: Set[String], y: Set[String]) => x intersect y)
      val boundKeys = lbs intersect ubs
      val bounds: Map[String, Iterable[(Value, Value)]] =
        boundKeys.map(b => (b -> components.map { case (_, be) => (be.lower.bounds(b), be.upper.bounds(b)) })).toMap
      val newBounds: Map[String, (Value, Value)] = bounds.flatMap { case (k, v) => boundsProduct(v).map(nv => (k -> nv)) }
      val newLowers = ListMap(newBounds.map { case (k, (l, _)) => (k -> l) }.toSeq: _*)
      val newUppers = ListMap(newBounds.map { case (k, (_, u)) => (k -> u) }.toSeq: _*)
      BellExpression(newExpr,
        LowerOrientation(newLowers, ListMap.empty[String, Boolean]),
        UpperOrientation(newUppers, ListMap.empty[String, Boolean])
      )
    }
  }

}

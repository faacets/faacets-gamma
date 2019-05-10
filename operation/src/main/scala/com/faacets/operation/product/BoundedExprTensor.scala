package com.faacets.operation.product
/*
import scala.collection.immutable.ListMap

import cats.instances.all._
import cats.syntax.traverse._
import spire.math.{Bounded, Interval, Point}
import cyclo.RealCyclo

import com.faacets.core.Expr
import com.faacets.data.{Scalar, Value}
import com.faacets.operation._

// see Proposition 5 of App. C in J. Phys. A: Math. Theor. 47 (2014) 424022
// the "facet" property is preserved when 1) facetOf(bound) is true 2) the corresponding bound is 0
case class IsFacet(lowerSatisfies: Boolean, upperSatisfies: Boolean) {
  def times(that: IsFacet): IsFacet = {
    // I >= 0 * J >= 0  =>  IJ >= 0
    // I <= 0 * J >= 0  =>  IJ <= 0
    // I >= 0 * J <= 0  =>  IJ <= 0
    // I <= 0 * J <= 0  =>  IJ >= 0
    // so l*l => l // l*u, u*l => u // u*u => l
    val newLS = (this.upperSatisfies && that.upperSatisfies) || (this.lowerSatisfies && that.lowerSatisfies)
    val newUS = (this.lowerSatisfies && that.upperSatisfies) || (this.upperSatisfies && that.lowerSatisfies)
    IsFacet(newLS, newUS)
  }

  def toOptionPair: (Option[Boolean], Option[Boolean]) = (
    if (lowerSatisfies) Some(true) else None,
    if (upperSatisfies) Some(true) else None
  )
}
object IsFacet {
  def apply(be: BoundedExpr, bound: String): IsFacet =
    IsFacet(be.lower.bounds.get(bound).fold(false)(_.isExactZero) && be.lower.facetOf.getOrElse(bound, false),
      be.upper.bounds.get(bound).fold(false)(_.isExactZero) && be.upper.facetOf.getOrElse(bound, false))
}

final class BoundedExprTensor(implicit ppb: ProductPreservedBounds) extends Tensor[BoundedExpr] {

  def exactBoundsProduct(components: Vector[Interval[RealCyclo]]): Interval[RealCyclo] =
    components.fold(Interval.point(RealCyclo.one))(_ * _)

  def boundsProduct(components: Vector[(Value, Value)]): Option[(Value, Value)] = {
    val exacts = components.map {
      case (Value(Point(Scalar.Exact(l))), Value(Point(Scalar.Exact(u)))) => Some(Interval(l, u))
      case _ => None
    }
    exacts.sequence.flatMap { vec =>
      exactBoundsProduct(vec) match {
        case Bounded(l, u, _) => Some((Value(l), Value(u)))
        case Point(x) => Some((Value(x), Value(x)))
        case _ => None
      }
    }
  }

  /** Computes whether the new expression is a facet of a particular set
    *
    * @param bound Bound name corresponding to the set (local, nonsignaling, ...)
    * @param components Components in the product
    * @return (isLowerFacetOf, isUpperFacetOf)
    */
  def isLowerOrUpperFacetOf(bound: String, components: Map[Set[Int], BoundedExpr]): (Option[Boolean], Option[Boolean]) =
    components.values.map(be => IsFacet(be, bound)).reduce(_ times _).toOptionPair

  def apply(components: Map[Set[Int], BoundedExpr]): BoundedExpr = {
    val exprs = components.mapValues(_.expr)
    val newExpr: Expr = Tensor[Expr].apply(exprs)
    if (components.size == 1) {
      val be = components.head._2
      BoundedExpr(newExpr,
        be.lower.processBounds(ppb.boundTransform).processFacetOf(ppb.facetOfTransform),
        be.upper.processBounds(ppb.boundTransform).processFacetOf(ppb.facetOfTransform),
      )
    } else {
      val lbs = components.map(_._2.lower.bounds.keySet).reduce((x: Set[String], y: Set[String]) => x intersect y)
      val ubs = components.map(_._2.upper.bounds.keySet).reduce((x: Set[String], y: Set[String]) => x intersect y)
      val boundKeys = lbs intersect ubs
      val bounds: Map[String, Iterable[(Value, Value)]] =
        boundKeys.map(b => (b -> components.map { case (_, be) => (be.lower.bounds(b), be.upper.bounds(b)) })).toMap
      val newBounds: Map[String, (Value, Value)] = bounds.flatMap { case (k, v) => boundsProduct(v.toVector).map(nv => (k -> nv)) }
      val newLowers = ListMap(newBounds.map { case (k, (l, _)) => (k -> l) }.toSeq: _*)
      val newUppers = ListMap(newBounds.map { case (k, (_, u)) => (k -> u) }.toSeq: _*)
      val luFacetOf = ListMap(boundKeys.toVector.map( bound => bound -> isLowerOrUpperFacetOf(bound, components) ): _*)
      val lFacetOf = luFacetOf.flatMap { case (k, (ov, _)) => ov.map( k -> _ ) }
      val uFacetOf = luFacetOf.flatMap { case (k, (_, ov)) => ov.map( k -> _ ) }

      BoundedExpr(newExpr,
        LowerOrientation(newLowers, lFacetOf),
        UpperOrientation(newUppers, uFacetOf)
      )
    }
  }

}
*/
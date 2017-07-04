package com.faacets.operation.product

import com.faacets.core.Expr
import com.faacets.data.{Scalar, Value}
import com.faacets.operation.{BoundedExpr, LowerOrientation, Tensor, UpperOrientation}
import cyclo.RealCyclo
import spire.math.{Bounded, Interval, Point}
import cats.syntax.traverse._
import cats.instances.all._

import scala.collection.immutable.ListMap

final class BoundedExprTensor extends Tensor[BoundedExpr] {

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

  def apply(components: Map[Set[Int], BoundedExpr]): BoundedExpr = {
    val exprs = components.mapValues(_.expr)
    val newExpr: Expr = Tensor[Expr].apply(exprs)
    if (components.size == 1) {
      val be = components.head._2
      BoundedExpr(newExpr,
        be.lower.filterBoundsAndFacetOf(BoundedExpr.stdPreserved),
        be.upper.filterBoundsAndFacetOf(BoundedExpr.stdPreserved)
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
      BoundedExpr(newExpr,
        LowerOrientation(newLowers, ListMap.empty[String, Boolean]),
        UpperOrientation(newUppers, ListMap.empty[String, Boolean])
      )
    }
  }

}

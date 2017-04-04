package com.faacets.operation

import com.faacets.core.{Behavior, DExpr, Expr, Scenario}
import com.faacets.operation.product.PVecTensorProduct
import net.alasc.domains.Partition
import spire.algebra.partial.Groupoid
import spire.util.Opt

trait TensorProduct[V] {
  /** Constructs an expression from a tensor product, possibly unordered.
    *
    * @param partition   Partition of the parties 0 ... p - 1 into blocks corresponding to expressions.
    * @param expressions Expressions, vector of length `partition.nBlocks`
    */
  def apply(partition: Partition, expressions: Vector[V]): V
}

object TensorProduct {
  def apply[V](implicit V: TensorProduct[V]): TensorProduct[V] = V
  implicit val expr: TensorProduct[Expr] = new PVecTensorProduct[Expr]
  implicit val dExpr: TensorProduct[DExpr] = new PVecTensorProduct[DExpr]
  implicit val behavior: TensorProduct[Behavior] = new PVecTensorProduct[Behavior]
}


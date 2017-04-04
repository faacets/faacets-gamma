package com.faacets.operation

import com.faacets.core.{Behavior, DExpr, Expr, Scenario}
import com.faacets.operation.product.PVecTensor
import net.alasc.domains.Partition
import spire.algebra.partial.Groupoid
import spire.util.Opt

trait Tensor[V] {
  /** Constructs an expression from a tensor product, possibly unordered.
    *
    * @param partition   Partition of the parties 0 ... p - 1 into blocks corresponding to expressions.
    * @param expressions Expressions, vector of length `partition.nBlocks`
    */
  def apply(partition: Partition, expressions: Vector[V]): V
}

object Tensor {
  def apply[V](implicit V: Tensor[V]): Tensor[V] = V
  implicit val expr: Tensor[Expr] = new PVecTensor[Expr]
  implicit val dExpr: Tensor[DExpr] = new PVecTensor[DExpr]
  implicit val behavior: Tensor[Behavior] = new PVecTensor[Behavior]
}


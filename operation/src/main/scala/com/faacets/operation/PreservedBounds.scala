package com.faacets.operation

import com.faacets.core.Relabeling
import com.faacets.data.Value

case class BoundRules(forProduct: ProductPreservedBounds,
                      forLifting: PreservedBounds[Lifting],
                      forReordering: PreservedBounds[Reordering],
                      forRelabeling: PreservedBounds[Relabeling])

object BoundRules {

  implicit val standardBoundRules: BoundRules = BoundRules(
      ProductPreservedBounds("local", "quantum", "nonsignaling"),
      PreservedBounds[Lifting]("local", "quantum", "nonsignaling"),
      PreservedBounds[Reordering]("local", "quantum", "nonsignaling"),
      PreservedBounds[Relabeling]("local", "quantum", "nonsignaling")
  )

}

case class ProductPreservedBounds(names: Set[String]) {
  def boundTransform(name: String, value: Value): Option[(String, Value)] =
    if (names.contains(name)) Some((name, value)) else None
  def facetOfTransform(name: String, isFacet: Boolean): Option[(String, Boolean)] =
    if (names.contains(name)) Some((name, isFacet)) else None
}

object ProductPreservedBounds {

  def apply(names: String*): ProductPreservedBounds = ProductPreservedBounds(Set(names: _*))

  implicit def fromBoundRules(implicit br: BoundRules): ProductPreservedBounds = br.forProduct

}

/** Lists the bounds that are preserved under transformations for a given operation.
  *
  * Concerns the bound value and the facetOf property.
  */
case class PreservedBounds[O](names: Set[String]) {
  def boundTransform(name: String, value: Value): Option[(String, Value)] =
    if (names.contains(name)) Some((name, value)) else None
  def facetOfTransform(name: String, isFacet: Boolean): Option[(String, Boolean)] =
    if (names.contains(name)) Some((name, isFacet)) else None
}

object PreservedBounds {

  def apply[O](names: String*): PreservedBounds[O] = PreservedBounds[O](Set(names: _*))

  implicit def fromBoundRulesLifting(implicit br: BoundRules): PreservedBounds[Lifting] = br.forLifting

  implicit def fromBoundRulesReordering(implicit br: BoundRules): PreservedBounds[Reordering] = br.forReordering

  implicit def fromBoundRulesRelabeling(implicit br: BoundRules): PreservedBounds[Relabeling] = br.forRelabeling

}

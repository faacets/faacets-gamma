package com.faacets
package operation
package relabeling
import spire.syntax.group._
import spire.syntax.action._
import spire.syntax.partialOrder._

import core._
import relabeling._

/*
      override def actrIsDefined(v: V, r: Relabeling): Boolean = ShapeLattice(r) <= v.scenario.shapeLattice
      override def actlIsDefined(r: Relabeling, v: V): Boolean = actrIsDefined(v, r.inverse)

      def partialActr(v: V, r: Relabeling): Nullbox[V] = if (actrIsDefined(v, r)) Nullbox(actr(v, r)) else Nullbox.empty[V]
      def partialActl(r: Relabeling, v: V): Nullbox[V] = partialActr(v, r.inverse)

      override def actr(v: V, r: Relabeling): V = {
        def newSymGrpOption: Option[Grp[Relabeling]] = v.symmetryGroupIfComputed.map(sym => sym.conjBy(InversePair(r, r.inverse)))
        v.representation match {
          case SPRepresentation | NPRepresentation =>
            implicit val action = v.scenario.probabilityAction
            v.builder(v.scenario, v.representation, v.coefficients <|+| r, newSymGrpOption)
          case WRepresentation =>
            implicit val action = v.scenario.strategyAction
            v.builder(v.scenario, v.representation, v.coefficients <|+| r, newSymGrpOption)
          case TRepresentation =>
            actr(v.to(WRepresentation), r).to(v.representation)
          case SCRepresentation | SGRepresentation =>
            actr(v.to(SPRepresentation), r).to(v.representation)
          case NCRepresentation | NGRepresentation =>
            actr(v.to(NPRepresentation), r).to(v.representation)
        }
      }

      override def actl(r: Relabeling, v: V): V = actr(v, r.inverse)

 */
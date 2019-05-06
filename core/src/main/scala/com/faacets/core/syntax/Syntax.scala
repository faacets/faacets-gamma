package com.faacets.core
package syntax

import shapeless.Witness

import net.alasc.finite.Grp
import com.faacets.core.{PartyRelabeling, PartyRelabelingSubgroups, Relabeling, RelabelingSubgroups}

trait SubgroupsSyntax {

  implicit def coreSubgroupsPartyRelabeling(grp: Grp[PartyRelabeling]): PartyRelabelingSubgroups = PartyRelabelingSubgroups(grp)

  implicit def coreSubgroupsRelabeling[S <: Scenario with Singleton:Witness.Aux](grp: Grp[S#Relabeling]): RelabelingSubgroups[S] =
    RelabelingSubgroups[S](grp)

}

trait LiteralsSyntax {

  implicit def coreLiterals(sc: StringContext): Literals = new Literals(sc)

}

trait AllSyntax
  extends LiteralsSyntax
  with SubgroupsSyntax

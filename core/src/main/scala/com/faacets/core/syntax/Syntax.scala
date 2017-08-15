package com.faacets.core.syntax

import net.alasc.finite.Grp

import com.faacets.core.{PartyRelabeling, PartyRelabelingSubgroups, Relabeling, RelabelingSubgroups}

trait SubgroupsSyntax {

  implicit def coreSubgroupsPartyRelabeling(grp: Grp[PartyRelabeling]): PartyRelabelingSubgroups = PartyRelabelingSubgroups(grp)

  implicit def coreSubgroupsRelabeling(grp: Grp[Relabeling]): RelabelingSubgroups = RelabelingSubgroups(grp)

}

trait LiteralsSyntax {

  implicit def coreLiterals(sc: StringContext): Literals = new Literals(sc)

}

trait AllSyntax
  extends LiteralsSyntax
  with SubgroupsSyntax

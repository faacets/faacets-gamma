package com.faacets
package core
package perm

import spire.algebra.Group
import net.alasc.perms._
import net.alasc.syntax.all._

import fastparse.noApi._

object Parsers {

  import com.faacets.core.Parsers._
  import com.faacets.data.Parsers._
  import White._

  val inputIndex = nonNegativeInt.opaque("<input-index>")

  def partiesSingleCycle: P[Cycles] = P( "(" ~ partyIndex.rep(sep=",") ~ ")" ).map { seq =>
    if (seq.isEmpty) Cycles.id else (Cycle(seq: _*): Cycles)
  }

  def relabelingPartyComponent: P[Relabeling] =
    P( partiesSingleCycle ).map( c => ref.PartyComponent(c.toPerm).toRelabeling )

  def relabelingInputComponent: P[Relabeling] = P( partyIndex ~ perm ).map {
    case (p, x) => ref.InputComponent(p, x).toRelabeling
  }

  def relabelingOutputComponent: P[Relabeling] = P( partyIndex ~ inputIndex ~/ perm ).map {
    case (p, x, a) => ref.OutputComponent(p, x, a).toRelabeling
  }

  def relabelingComponent: P[Relabeling] =
    P( relabelingPartyComponent | relabelingInputComponent | relabelingOutputComponent )

  def relabeling: P[Relabeling] = relabelingComponent.rep.map( Group[Relabeling].combineAll(_) )

  def partyRelabelingOutputComponent: P[PartyRelabeling] = P( inputIndex ~ perm ).map {
    case (x, a) => PartyRelabeling.OutputComponent(x, a).get
  }

  def partyRelabelingInputComponent: P[PartyRelabeling] = P( "I" ~/ perm ).map( PartyRelabeling.InputComponent(_).get )

  def partyRelabelingComponent: P[PartyRelabeling] = P( partyRelabelingOutputComponent | partyRelabelingInputComponent )

  def partyRelabeling: P[PartyRelabeling] = partyRelabelingComponent.rep.map( Group[PartyRelabeling].combineAll(_) )

}

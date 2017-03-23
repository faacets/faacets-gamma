package com.faacets
package core
package perm

import spire.syntax.group._

import net.alasc.syntax.permutationAction._

import data._
import fastparse.noApi._

class PartyRelabelingParsable extends Parsable[PartyRelabeling] {

  import com.faacets.data.Parsers._
  import Parsers._
  import White._

  val phrase: P[PartyRelabeling] = partyRelabeling ~ End

  def toText(pr: PartyRelabeling): String = pr.toString

}

package com.faacets
package data

import spire.algebra.Group
import spire.syntax.group._

import net.alasc.perms.{Cycle, Cycles, Perm}
import net.alasc.syntax.permutationAction._
import consolidate.Merge
import fastparse.WhitespaceApi
import fastparse.noApi._

final class PermParsable extends Parsable[Perm] {

  import Parsers._
  import White._

  val phrase: P[Perm] = perm ~ End

  def toText(p: Perm) = if (p.isId) "id" else p.toCycles.string

}

trait PermInstances {

  implicit val permParsable: Parsable[Perm] = new PermParsable

  implicit val permMerge: Merge[Perm] = Merge.fromEquals[Perm]

}

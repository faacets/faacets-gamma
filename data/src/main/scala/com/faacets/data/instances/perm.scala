package com.faacets.data.instances

import com.faacets.consolidate.Merge
import com.faacets.data.{Parsable, Parsers}
import fastparse.noApi._

import net.alasc.perms.Perm

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

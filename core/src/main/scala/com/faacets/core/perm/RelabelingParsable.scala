package com.faacets
package core
package perm

import data._
import fastparse.noApi._

class RelabelingParsable extends Parsable[Relabeling] {

  import com.faacets.data.Parsers._
  import Parsers._
  import White._

  val phrase: P[Relabeling] = relabeling ~ End

  def toText(r: Relabeling): String = r.toString

}

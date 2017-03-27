package com.faacets.core
package syntax

import com.faacets.core.PartyRelabeling
import com.faacets.data.syntax.textable._

class Literals(val sc: StringContext) extends AnyVal {

  def rel(): Relabeling = sc.parts.mkString.parseUnsafe[Relabeling]

  def prel(): PartyRelabeling = sc.parts.mkString.parseUnsafe[PartyRelabeling]

}

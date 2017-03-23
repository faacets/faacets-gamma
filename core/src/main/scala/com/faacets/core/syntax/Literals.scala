package com.faacets.core
package syntax

import perm.Relabeling

import com.faacets.data.Textable.syntax._

class Literals(val sc: StringContext) extends AnyVal {

  def rel(): Relabeling = sc.parts.mkString.parseUnsafe[Relabeling]

}

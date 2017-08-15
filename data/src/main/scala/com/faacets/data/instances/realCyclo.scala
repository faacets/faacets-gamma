package com.faacets.data
package instances

import cyclo.RealCyclo

import com.faacets.consolidate.Merge

trait RealCycloInstances {

  implicit val realCycloMerge: Merge[RealCyclo] = Merge.fromEquals[RealCyclo]

  implicit val realCycloTextable: Textable[RealCyclo] = Textable.fromParser(Parsers.realCyclo.realCyclo, _.toString)

}

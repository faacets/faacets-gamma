package com.faacets.data
package instances

import com.faacets.consolidate.Merge
import cyclo.RealCyclo

trait RealCycloInstances {

  implicit val realCycloMerge: Merge[RealCyclo] = Merge.fromEquals[RealCyclo]

  implicit val realCycloTextable: Textable[RealCyclo] = Textable.fromParser(Parsers.realCyclo.realCyclo, _.toString)

}

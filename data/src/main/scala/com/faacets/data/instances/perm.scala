package com.faacets.data.instances

import net.alasc.perms.Perm

import com.faacets.consolidate.Merge
import com.faacets.data.{Parsers, Textable}

trait PermInstances {

  implicit val permTextable: Textable[Perm] =
    Textable.fromParser[Perm](Parsers.perm, p => if(p.isId) "id" else p.toCycles.string)

  implicit val permMerge: Merge[Perm] = Merge.fromEquals[Perm]

}

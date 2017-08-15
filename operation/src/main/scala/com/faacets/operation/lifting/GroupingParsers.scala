package com.faacets
package operation
package lifting

import net.alasc.partitions.Partition

import fastparse.noApi._

object GroupingParsers {

  import com.faacets.data.Parsers._
  import White._

  val inputNoGrouping: P[InputGrouping] = int.map( InputGrouping.noLifting(_) )

  val inputWithGrouping: P[InputGrouping] = P( "{" ~ int.rep(min=1) ~ "}" ).map { outputs =>
      val blocks: Iterable[Set[Int]] = outputs.zipWithIndex.groupBy(_._1).map { case (k, v) => v.map(_._2).toSet }
      InputGrouping(Partition(blocks.toSeq: _*))
  }

  val inputGrouping: P[InputGrouping] = P( inputNoGrouping | inputWithGrouping )

  val partyGrouping: P[PartyGrouping] = P( "(" ~ inputGrouping.rep(min=1) ~ ")" ).map(PartyGrouping(_))

  val grouping: P[Grouping] = P( "[" ~ partyGrouping.rep(min=1) ~ "]").map(Grouping(_))

}

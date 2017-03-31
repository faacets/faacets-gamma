package com.faacets.core

import fastparse.noApi._

object Parsers {

  import com.faacets.data.Parsers._

  import White._

  val numberOfOutputs = positiveInt.opaque("<number-of-outputs>")

  val partyIndex: P[Int] = P( CharIn('A'to'Z').! ).opaque("<party-letter>").map( _.head - 'A' )

  val party: P[Party] = P( "(" ~ numberOfOutputs.rep(min=1) ~ ")" ).map( seq => Party(seq) )

  val scenario: P[Scenario] = P( "[" ~ party.rep(min=1) ~ "]" ).map(Scenario(_))

}

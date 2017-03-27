package com.faacets
package laws

import org.scalacheck._
import net.alasc.laws._
import core._

object Scenarios {

  trait ScenarioGenerators {

    def genScenario: Gen[Scenario]

    implicit def arbScenario: Arbitrary[Scenario] = Arbitrary(genScenario)

  }
/* TODO
  object Tiny extends ScenarioGenerators {

    def genScenario = Small.genScenario.filter(_.strategyRep.dimension < 30)

  }*/

  object Small extends ScenarioGenerators {

    def genScenario = for {
      nParties <- Gen.choose(1, 3)
      parties <- Gen.containerOfN[Seq, Party](nParties, Parties.Small.genParty)
    } yield Scenario(parties)

  }


  object Large extends ScenarioGenerators {

    def genScenario = for {
      p <- Gen.choose(1, 5)
      parties <- Gen.containerOfN[Seq, Party](p, Parties.Large.genParty)
    } yield Scenario(parties)

  }

  implicit val scenarioInstances: Instances[Scenario] = Instances(Seq(Scenario.nmk(3,2,2), Scenario.nmk(2,3,2)))

  implicit val scenarioCloner: Cloner[Scenario] = Cloner(scenario => Scenario(scenario.parties.map(_.inputs).map(xs => Party(xs))))

}

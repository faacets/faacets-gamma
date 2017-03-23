package com.faacets
package laws

import org.scalacheck._
import net.alasc.laws._
import data._
import core._

object Parties {

  trait PartyGenerators {

    def genParty: Gen[Party]

    implicit def arbParty: Arbitrary[Party] = Arbitrary(genParty)

  }

  object Small extends PartyGenerators {

    def genParty = for {
      nInputs <- Gen.choose(1, 3)
      outputs <- Gen.containerOfN[Seq, Int](nInputs, Gen.choose(2, 3))
    } yield Party(outputs)

  }

  object Large extends PartyGenerators {

    def genParty = for {
      nInputs <- Gen.choose(1, 5)
      outputs <- Gen.containerOfN[Seq, Int](nInputs, Gen.choose(2, 5))
    } yield Party(outputs)

  }

  object Huge extends PartyGenerators {

    def genParty = for {
      nInputs <- Gen.choose(1, 11)
      outputs <- Gen.containerOfN[Seq, Int](nInputs, Gen.choose(2, 11))
    } yield Party(outputs)

  }

  implicit val partyInstances: Instances[Party] = Instances(Seq(Party(Seq(3,2)), Party(Seq(2, 3))))

  implicit val partyCloner: Cloner[Party] = Cloner(party => Party(party.inputs))

}

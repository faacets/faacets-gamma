package com.faacets
package laws

import org.scalacheck._
import net.alasc.laws._
import core._

object Parties {

  trait PartyGenerators {

    def genParty: Gen[Party]

    implicit def arbParty: Arbitrary[Party] = Arbitrary(genParty)

  }

  object Small extends PartyGenerators {

    def genParty = Parties.genParty(outputs = 2 -> 3, inputs = 1 -> 3) // TODO 1 -> 3 outputs

  }

  object Large extends PartyGenerators {

    def genParty = Parties.genParty(outputs = 2 -> 5, inputs = 1 -> 5)

  }

  object Huge extends PartyGenerators {

    def genParty = Parties.genParty(outputs = 1 -> 11, inputs = 1 -> 11)

  }

  def genParty(outputs: (Int, Int), inputs: (Int, Int)): Gen[Party] = for {
    nInputs <- Gen.choose(inputs._1, inputs._2)
    outputs <- Gen.containerOfN[Seq, Int](nInputs, Gen.choose(outputs._1, outputs._2))
  } yield Party(outputs)

  implicit val partyInstances: Instances[Party] = Instances(Seq(Party(Seq(3,2)), Party(Seq(2, 3))))

  implicit val partyCloner: Cloner[Party] = Cloner(party => Party(party.inputs))

}

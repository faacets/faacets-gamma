package com.faacets
package operation
package relabeling
/*
import org.scalacheck._

import spire.algebra.Action
import spire.syntax.group._
import spire.syntax.action._
import spire.std.int._

import net.alasc.math.enum.RepresentativesOrdered
import net.alasc.syntax.all._
import net.alasc.std.seq._

import core._
import core.laws.Parties.Large.genParty
import perm.PartyRelabeling

object MarginalsGenerators {
  def genPartyRelabeling(party: Party): Gen[PartyRelabeling] = Gen.parameterized { params => party.group.randomElement(params.rng) }
  val genPartyMarginalsPerm = for {
    party <- genParty
    vector <- Gen.containerOfN[Seq, Int](party.shapeP.size, Gen.choose(0, 5))
    perm <- genPartyRelabeling(party)
  } yield (party, vector, perm)
}

object SinglePartySpec extends Properties("SingleParty") {
  import core.laws.Parties.Large._
  import core.laws.PartyRelabelings.genPartyRelabeling

  def genSeqInt(party: Party): Gen[Seq[Int]] =
    Gen.containerOfN[Seq, Int](party.shapeP.size, Gen.choose(0, 5))

  property("Minimal form is the same for marginals and its random permutation") =
    Prop.forAll { party: Party =>
      Prop.forAll(genPartyRelabeling(party)) { pr =>
        Prop.forAll(genSeqInt(party)) { coeffs1 =>
          implicit def permutationAction = party.probabilityAction

          val coeffs2 = coeffs1 <|+| pr
          val firstLex1 =
            RepresentativesOrdered(coeffs1, party.group, party.probabilityRepresentation).head.get
          val firstLex2 = coeffs1 <|+| SingleParty.findMinimalPermutation(party)(coeffs1).inverse
          val firstLex3 = coeffs2 <|+| SingleParty.findMinimalPermutation(party)(coeffs2).inverse

          (firstLex1 == firstLex2) && (firstLex2 == firstLex3)
        }
      }
    }
}

*/
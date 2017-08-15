package com.faacets
package operation
package relabeling

import spire.std.int._
import net.alasc.perms.default._
import net.alasc.std.seq._

import org.scalacheck._

import com.faacets.core._

class SinglePartyTest extends FaacetsSuite {

  import com.faacets.laws.PartyRelabelings.genPartyRelabeling

  def genSeqInt(party: Party): Gen[Seq[Int]] =
    Gen.containerOfN[Seq, Int](party.shapeP.size, Gen.choose(0, 5))

  test("Minimal form is the same for marginals and its random permutation") {
    import com.faacets.laws.Parties.Large.genParty

    forAll(genParty) { party =>
     forAll(genPartyRelabeling(party)) { pr =>
        forAll(genSeqInt(party)) { coeffs1 =>
          implicit def permutationAction = party.probabilityAction
          val coeffs2 = coeffs1 <|+| pr
          val firstLex1 = coeffs1 <|+| net.alasc.perms.orbits.Seqs.Representatives.ordered(party.group, permutationAction, coeffs1).minimum
          val firstLex2 = coeffs1 <|+| SingleParty.findMinimalPermutation(party)(coeffs1).inverse
          val firstLex3 = coeffs2 <|+| SingleParty.findMinimalPermutation(party)(coeffs2).inverse

          (firstLex1 == firstLex2) && (firstLex2 == firstLex3)
        }
      }
    }
  }

}

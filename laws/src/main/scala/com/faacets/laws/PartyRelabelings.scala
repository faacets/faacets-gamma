package com.faacets.laws

import com.faacets.core.Party
import com.faacets.core.perm.PartyRelabeling
import org.scalacheck._

import net.alasc.laws._
import net.alasc.perms.{GrpFixingPartition, Perm}
import net.alasc.perms.default._

object PartyRelabelings {

  def genOutputPerms(
                      reverseInputs: List[Int],
                      acc: List[Gen[Perm]]): Gen[List[Perm]] = reverseInputs match {
    case nOutputs :: tail =>
      genOutputPerms(tail, Permutations.permForSize(nOutputs) :: acc)
    case Nil => Gen.sequence[List[Perm], Perm](acc)
  }

  def genPartyRelabeling(party: Party): Gen[PartyRelabeling] = for {
    aSeq <- genOutputPerms(party.inputs.reverse.toList, Nil)
    xPerm <- Grps.genRandomElement(GrpFixingPartition(party.shapeLattice.pm.partition))
  } yield PartyRelabeling(aSeq, xPerm)

  implicit def arbPartyRelabelingInParty(implicit party: Party): Arbitrary[PartyRelabeling] =
    Arbitrary(genPartyRelabeling(party))

  implicit def arbPartyRelabeling(implicit P: Arbitrary[Party]) =
    Arbitrary(P.arbitrary.flatMap(genPartyRelabeling(_)))

  implicit val partyRelabelingInstances: Instances[PartyRelabeling] = Instances(Seq(
    PartyRelabeling(Map(0 -> Perm(0, 1)), Perm(0, 1)),
    PartyRelabeling(Map(1 -> Perm(0, 2)), Perm(1, 2))
  ))

  implicit val partyRelabelingCloner: Cloner[PartyRelabeling] = Cloner( (pr: PartyRelabeling) =>
    PartyRelabeling(pr.outputRelabelingMap, pr.xPerm) )

}

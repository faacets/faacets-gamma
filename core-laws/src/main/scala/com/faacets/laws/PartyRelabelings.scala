package com.faacets.laws

import net.alasc.laws._
import net.alasc.partitions.Partition
import net.alasc.perms.default._
import net.alasc.perms.{GrpFixingPartition, Perm}

import org.scalacheck._

import com.faacets.core.{Party, PartyRelabeling}

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
    xPerm <- Grps.genRandomElement(GrpFixingPartition(Partition.fromSeq(party.inputs)))
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

  def genPair(maxInputIndex: Int, maxOutputIndex: Int): Gen[(Int, Int)] = for {
    x <- Gen.choose(0, maxInputIndex)
    a <- Gen.choose(0, maxOutputIndex)
  } yield (x, a)

  implicit val arbPairs: Arbitrary[(Int, Int)] = Arbitrary(genPair(9, 9))


}

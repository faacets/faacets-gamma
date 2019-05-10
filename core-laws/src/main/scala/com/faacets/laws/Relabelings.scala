package com.faacets
package laws

import shapeless.Witness

import com.faacets.core.ref.{POVM, RefRelabeling}
import net.alasc.laws._
import net.alasc.partitions.Partition
import net.alasc.perms.default._
import net.alasc.perms.{GrpFixingPartition, Perm}
import org.scalacheck._
import com.faacets.core.{PVec, PartyRelabeling, Relabeling, Scenario, valueOf}

abstract class Relabelings0 {
  import Relabelings._

  implicit def arbRelabeling(implicit S: Arbitrary[Scenario]) =
    Arbitrary(S.arbitrary.flatMap(genRelabeling(_)))
}
object Relabelings extends Relabelings0 {
  import PartyRelabelings._

  def genRelabeling(scenario: Scenario): Gen[Relabeling.Aux[scenario.type]] = for {
    prSeq <- Gen.sequence[Seq[PartyRelabeling], PartyRelabeling](scenario.parties.map(genPartyRelabeling(_)))
    pPerm <- Grps.genRandomElement(GrpFixingPartition(scenario.partition))
  } yield Relabeling(prSeq, pPerm).inNC(scenario)


  implicit def arbRelabelingInScenario[S <: Scenario: Witness.Aux]: Arbitrary[Relabeling.Aux[S]] =
    Arbitrary(genRelabeling(valueOf[S]))

  implicit val relabelingInstances: Instances[Relabeling] = Instances(Seq(
    Relabeling(Map(0 -> PartyRelabeling(Map(0 -> Perm(0, 1)), Perm(0, 1))), Perm(0, 1)),
    Relabeling(Map(1 -> PartyRelabeling(Map(1 -> Perm(0, 2)), Perm(1, 2))), Perm.id)
  ))

  implicit val relabelingCloner: Cloner[Relabeling] = Cloner( (r: Relabeling) =>
    Relabeling(r.partyRelabelingMap.mapValues(partyRelabelingCloner.make(_)), r.pPerm)
  )

  def genPOVM(maxPartyIndex: Int, maxInputIndex: Int, maxOutputIndex: Int): Gen[POVM] = for {
    p <- Gen.choose(0, maxPartyIndex)
    x <- Gen.choose(0, maxInputIndex)
    a <- Gen.choose(0, maxOutputIndex)
  } yield POVM(p, x, a)

  implicit val arbPOVM: Arbitrary[POVM] = Arbitrary(genPOVM(25, 9, 9))

  implicit def arbRefRelabeling(implicit S: Arbitrary[Scenario]): Arbitrary[RefRelabeling] =
    Arbitrary { arbRelabeling(S).arbitrary.map(_.toRefRelabeling) }

}

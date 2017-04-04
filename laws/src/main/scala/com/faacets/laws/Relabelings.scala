package com.faacets
package laws

import com.faacets.core.{PVec, PartyRelabeling, Relabeling, Scenario}
import org.scalacheck._
import net.alasc.laws._
import net.alasc.perms.{GrpFixingPartition, Perm}
import net.alasc.perms.default._

object Relabelings {

  import PartyRelabelings._

  def genRelabeling(scenario: Scenario): Gen[Relabeling] = for {
    prSeq <- Gen.sequence[Seq[PartyRelabeling], PartyRelabeling](scenario.parties.map(genPartyRelabeling(_)))
    pPerm <- Grps.genRandomElement(GrpFixingPartition(scenario.shapeLattice.pm.partition))
  } yield Relabeling(prSeq, pPerm)

  implicit def arbRelabelingInScenario(implicit scenario: Scenario): Arbitrary[Relabeling] =
    Arbitrary(genRelabeling(scenario))

  implicit def arbRelabeling(implicit S: Arbitrary[Scenario]) =
    Arbitrary(S.arbitrary.flatMap(genRelabeling(_)))

  implicit val relabelingInstances: Instances[Relabeling] = Instances(Seq(
    Relabeling(Map(0 -> PartyRelabeling(Map(0 -> Perm(0, 1)), Perm(0, 1))), Perm(0, 1)),
    Relabeling(Map(1 -> PartyRelabeling(Map(1 -> Perm(0, 2)), Perm(1, 2))), Perm.id)
  ))

  implicit val relabelingCloner: Cloner[Relabeling] = Cloner( (r: Relabeling) =>
    Relabeling(r.partyRelabelingMap.mapValues(partyRelabelingCloner.make(_)), r.pPerm)
  )

  def genRelabeling[V <: PVec[V]](vec: V): Gen[Relabeling] = genRelabeling(vec.scenario)

// TODO  implicit val relabelingGenerator: OperationGenerator[Expr[_ <: Scenario with Singleton], Relabeling] =
//    OperationGenerator[Expr[_ <: Scenario with Singleton], Relabeling](genRelabeling)

}

package com.faacets.operation.product
/*
import spire.math.Rational
import spire.syntax.cfor._

import com.faacets.core.{PVec, PVecBuilder, Scenario}
import com.faacets.operation.{SetPartition, Tensor}

final class PVecTensor[V <: PVec[V]](implicit builder: PVecBuilder[V]) extends Tensor[V] {

  def apply(components: Map[Set[Int], V]): V = {
    val setPartition = SetPartition(components.keySet)
    import spire.std.int._
    val orderedParts: Seq[Seq[Int]] = setPartition.toSeqOfSeqs
    val orderedV = orderedParts.map(p => components(p.toSet))

    val nParties = setPartition.size
    val nParts = setPartition.parts.size
    val partyBlockIndex: Vector[(Int, Int)] = Vector(0 until nParties: _*).map { p =>
      val blockIndex: Int = orderedParts.indexWhere(_.contains(p))
      val indexInBlock = orderedParts(blockIndex).indexOf(p)
      (blockIndex, indexInBlock)
    }
    val oArrays = orderedV.map(e => new Array[Int](e.scenario.nParties) )
    val iArrays = orderedV.map(e => new Array[Int](e.scenario.nParties) )
    val finalScenario = Scenario(partyBlockIndex.map { case (b, i) => orderedV(b).scenario.parties(i) })
    val finalCoeffs = finalScenario.tabulateP { (outputs, inputs) =>
      cforRange(0 until nParties) { p =>
        val (b, i) = partyBlockIndex(p)
        oArrays(b)(i) = outputs(p)
        iArrays(b)(i) = inputs(p)
      }
      var coeff: Rational = Rational.one
      cforRange(0 until nParts) { b =>
        coeff *= orderedV(b).coefficient(oArrays(b), iArrays(b))
      }
      coeff
    }
    builder.apply(finalScenario, finalCoeffs)
  }

}
*/
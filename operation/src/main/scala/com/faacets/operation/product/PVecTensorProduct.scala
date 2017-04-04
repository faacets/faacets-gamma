package com.faacets.operation.product

import com.faacets.core.{PVec, PVecBuilder, Scenario}
import com.faacets.operation.TensorProduct
import net.alasc.domains.Partition
import spire.math.Rational
import spire.syntax.cfor._

final class PVecTensorProduct[V <: PVec[V]](implicit builder: PVecBuilder[V]) extends TensorProduct[V] {

  def apply(partition: Partition, expressions: Vector[V]): V = {
    val blocks = partition.blocks.map(_.toVector.sorted)
    val nParties = partition.size
    val nBlocks = partition.nBlocks
    val partyBlockIndex: Vector[(Int, Int)] = Vector(0 until nParties: _*).map { p =>
      val blockIndex = partition.blockIndex(p)
      val indexInBlock = blocks(blockIndex).indexOf(p)
      (blockIndex, indexInBlock)
    }
    val oArrays = expressions.map(e => new Array[Int](e.scenario.nParties) )
    val iArrays = expressions.map(e => new Array[Int](e.scenario.nParties) )
    val finalScenario = Scenario(partyBlockIndex.map { case (b, i) => expressions(b).scenario.parties(i) })
    val finalCoeffs = finalScenario.tabulateP { (outputs, inputs) =>
      cforRange(0 until nParties) { p =>
        val (b, i) = partyBlockIndex(p)
        oArrays(b)(i) = outputs(p)
        iArrays(b)(i) = inputs(p)
      }
      var coeff: Rational = Rational.one
      cforRange(0 until nBlocks) { b =>
        coeff *= expressions(b).coefficient(oArrays(b), iArrays(b))
      }
      coeff
    }
    builder.apply(finalScenario, finalCoeffs)
  }

}

package com.faacets.operation.reordering

import spire.algebra.Order

import com.faacets.core.Party

object LexicographicPartyOrder extends Order[Party] {

  def compare(a: Party, b: Party) = {
    val compareInputs = (a.inputs zip b.inputs).foldLeft(0) {
      case (0, (na, nb)) if na > nb => 1
      case (0, (na, nb)) if na < nb => -1
      case (res, _) => res
    }
    compareInputs match {
      case 0 => a.inputs.length - b.inputs.length
      case _ => compareInputs
    }
  }

}

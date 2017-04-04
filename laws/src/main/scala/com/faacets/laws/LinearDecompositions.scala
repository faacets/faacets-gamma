package com.faacets.laws

import com.faacets.core.Relabeling
import com.faacets.operation._
import org.scalacheck.Gen
import spire.algebra.partial.PartialAction
import spire.syntax.partialAction._

object LinearDecompositions {

  def genLinearDecomposition[V](canonical: V)(implicit
                                              A: PartialAction[V, Affine], AE: OperationExtractor[V, Affine], AG: Operations.Generator[V, Affine],
                                              L: PartialAction[V, Lifting], LE: OperationExtractor[V, Lifting], LG: Operations.Generator[V, Lifting],
                                              O: PartialAction[V, Reordering], OE: OperationExtractor[V, Reordering], OG: Operations.Generator[V, Reordering],
                                              R: PartialAction[V, Relabeling], RE: OperationExtractor[V, Relabeling], RG: Operations.Generator[V, Relabeling]
  ): Gen[LinearDecomposition[V]] =
  for {
    a <- AG.gen(canonical)
    r <- RG.gen(canonical)
    o <- OG.gen(canonical)
    after = (((canonical <|+|? a).get <|+|? r).get <|+|? o).get
    l <- LG.gen(after)
  } yield LinearDecomposition(a, l, o, r, canonical)

}

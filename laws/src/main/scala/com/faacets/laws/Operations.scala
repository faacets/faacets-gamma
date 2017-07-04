package com.faacets.laws

import com.faacets.core.Relabeling
import com.faacets.operation.CanonicalDecWithAffine
import com.faacets.operation.{Affine, Lifting, OperationExtractor, Reordering}
import spire.algebra.partial.PartialAction
import spire.syntax.partialAction._
import spire.syntax.groupoid._

object Operations {

  import org.scalacheck.Gen

  case class Generator[V, O](gen: V => Gen[O])

  def genCanonicalWithAffine[V](canonical: V)(implicit
                                              A: PartialAction[V, Affine], AE: OperationExtractor[V, Affine], AG: Generator[V, Affine],
                                              L: PartialAction[V, Lifting], LE: OperationExtractor[V, Lifting], LG: Generator[V, Lifting],
                                              O: PartialAction[V, Reordering], OE: OperationExtractor[V, Reordering], OG: Generator[V, Reordering],
                                              R: PartialAction[V, Relabeling], RE: OperationExtractor[V, Relabeling], RG: Generator[V, Relabeling]
  ): Gen[CanonicalDecWithAffine[V]] =
    for {
      a <- AG.gen(canonical)
      r <- RG.gen(canonical)
      o <- OG.gen(canonical)
      after = (((canonical <|+|? a).get <|+|? r).get <|+|? o).get
      l <- LG.gen(after)
    } yield CanonicalDecWithAffine(a, if (l.isId) None else Some(l), if (o.isId) None else Some(o), r, canonical)

}

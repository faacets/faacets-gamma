package com.faacets.laws

import spire.algebra.partial.PartialAction
import spire.syntax.groupoid._
import spire.syntax.partialAction._
import com.faacets.core.{PVec, Relabeling}
import com.faacets.laws.Relabelings.genRelabeling
import com.faacets.operation._

object Operations {

  import org.scalacheck.Gen

  case class Generator[V, O](gen: V => Gen[O])

  object Generator {
    implicit def relabelingGenerator[V <: PVec[V]]: Operations.Generator[V, Relabeling] =
      Operations.Generator[V, Relabeling](genRelabeling(_))
  }

  def genCanonicalWithAffine[V](canonical: V)(implicit
                                              A: PartialAction[V, Affine], AG: Generator[V, Affine],
                                              LG: Generator[V, Lifting],
                                              O: PartialAction[V, Reordering], OG: Generator[V, Reordering],
                                              R: PartialAction[V, Relabeling], RG: Generator[V, Relabeling]
  ): Gen[CanonicalDecWithAffine[V]] =
    for {
      a <- AG.gen(canonical)
      r <- RG.gen(canonical)
      o <- OG.gen(canonical)
      after = (((canonical <|+|? a).get <|+|? r).get <|+|? o).get
      l <- LG.gen(after)
    } yield CanonicalDecWithAffine(a, if (l.isId) None else Some(l), if (o.isId) None else Some(o), r, canonical)

}

package com.faacets.core.perm

import org.scalacheck._

import com.faacets.FaacetsSuite

class PrimitiveShapeSuite extends FaacetsSuite {

  def primitiveShapeGen: Gen[PrimitiveShape] =
    for {
      n <- Gen.choose(1, 5)
      array <- Gen.containerOfN[Array, Int](n, Gen.choose(2, 10))
    } yield PrimitiveShape(array)

  def primitiveShapeAndIndexGen: Gen[(PrimitiveShape, Int)] =
    for {
      primitiveShape <- primitiveShapeGen
      index <- Gen.choose(0, primitiveShape.size - 1)
    } yield (primitiveShape, index)

  test("Conversion to sub-indices and back") {

    forAll(primitiveShapeAndIndexGen) {
      case (primitiveShape: PrimitiveShape, ind: Int) =>
        val sub = new Array[Int](primitiveShape.n)
        primitiveShape.ind2sub(ind, sub)
        primitiveShape.sub2ind(sub) == ind
    }

  }

}

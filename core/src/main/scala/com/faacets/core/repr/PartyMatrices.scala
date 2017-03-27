package com.faacets
package core
package repr

import spire.math.Rational

import scalin.immutable.Mat

import scalin.immutable.dense._

import scalin.syntax.all._

import spire.syntax.cfor._

/** Party representations methods */
case class PartyMatrices(party: Party) {

  import party._

  /*
  implicit class MutableMatRational(val mat: scalin.mutable.Mat[Rational]) {

    def compact(): Unit = {
      val set: metal.mutable.HashSet[Rational] = metal.mutable.HashSet.empty[Rational]
      cforRange(0 until mat.nRows) { r =>
        cforRange(0 until mat.nCols) { c =>
          val v = mat(r, c)
          val ptr: Ptr[set.type] = set.ptrFind[Rational](v)
          if (ptr.nonNull)
            mat(r, c) := ptr.get.key
          else
            set.add(v)
        }
      }
    }

  }*/

  lazy val matSCfromSP: Mat[Rational] = {
    val d = inputs.sum
    val n = inputs.length
    Mat.fromMutable(d, d, Rational.zero) { m =>
      m(0, ::) := Rational(1, n)
      var r = 1
      var c = 0
      cforRange(0 until n) { i =>
        val o = inputs(i)
        val q = Q(o).matrix
        m(r until r + o - 1, c until c + o) := q(1 until o, ::)
        r += o - 1
        c += o
      }
      val q = Q(n).matrix
      cforRange(1 until n) { i =>
        c = 0
        cforRange(0 until n) { j =>
          val o = inputs(j)
          m(r, c until c + o) := q(i, j)
          c += o
        }
        r += 1
      }
    }
  }

  lazy val matSPfromSC: Mat[Rational] = {
    val d = inputs.sum
    val n = inputs.length
    Mat.fromMutable(d, d, Rational.zero) { m =>
      var r = 0
      var c = 1
      cforRange(0 until n) { i =>
        val o = inputs(i)
        val qinv = Q(o).matrixInverse
        m(r until r + o, 0) := Rational(1, o)
        m(r until r + o, c until c + o - 1) := qinv(::, 1 until o)
        r += o
        c += o - 1
      }
      val qinv = Q(n).matrixInverse
      cforRange(1 until n) { j =>
        r = 0
        cforRange(0 until n) { i =>
          val o = inputs(i)
          m(r until r + o, c) := qinv(i, j) / o
          r += o
        }
        c += 1
      }
    }
  }

  lazy val matSGfromSP: Mat[Rational] = {
    val n = inputs.length
    val d = inputs.sum
    Mat.fromMutable(d, d, Rational.zero) { m =>
      m(0, ::) := Rational(1, n)
      var c = 0
      var r = 1
      cforRange(0 until n) { i =>
        val o = inputs(i)
        val a = Rational(-(n - 1), o * n)
        val b = Rational(1, o * n)
        m(r until r + o - 1, ::) := b
        cforRange(0 until inputs(i) - 1) { dr =>
          cforRange(0 until inputs(i)) { dc =>
            m(r + dr, c + dc) := a + (if (dr == dc) 1 else 0)
          }
        }
        r += o - 1
        c += o
      }
      val q = Q(n).matrix
      cforRange(1 until n) { i =>
        c = 0
        cforRange(0 until n) { j =>
          val o = inputs(j)
          m(r, c until c + o) := q(i, j)
          c += o
        }
        r += 1
      }
    }
  }

  lazy val matSPfromSG: Mat[Rational] = {
    val n = inputs.length
    val d = inputs.sum
    Mat.fromMutable(d, d, Rational.zero) { m =>
      var c = 1
      var r = 0
      cforRange(0 until n) { i =>
        val o = inputs(i)
        m(r + o - 1, 0) := Rational.one
        m(r + o - 1, c until c + o - 1) := -Rational.one
        val idominus1 = Mat.eye[Rational](o - 1)
        m(r until r + o - 1, c until c + o - 1) := idominus1
        r += o
        c += o - 1
      }
      val qinv = Q(n).matrixInverse
      cforRange(1 until n) { j =>
        r = 0
        cforRange(0 until n) { i =>
          val o = inputs(i)
          m(r until r + o, c) := qinv(i, j) / o
          r += o
        }
        c += 1
      }
    }
  }

  lazy val matSPfromW: Mat[Rational] = {
    val n = inputs.length
    val d = inputs.sum
    val p = inputs.product
    Mat.fromMutable(d, p, Rational.zero) { m =>
      cforRange(0 until p) { i =>
        var r = 0
        var t = i
        cforRange(0 until n) { j =>
          val o = inputs(j)
          val alpha = t % o
          t = t / o
          m(r + alpha, i) := Rational.one
          r += o
        }
      }
    }
  }

  def matSCfromNC: Mat[Rational] = matSGfromNG

  def matNCfromSC: Mat[Rational] = matNGfromSG

  lazy val matProjectionInSG: Mat[Rational] = matSGfromNG * matNGfromSG

  def matProjectionInSC: Mat[Rational] = matProjectionInSG

  lazy val matProjectionInSP: Mat[Rational] = matSPfromSG * matProjectionInSG * matSGfromSP

  lazy val matNCfromT: Mat[Rational] = {
    val n = inputs.length
    val s = inputs.sum
    val d = s - n + 1
    val p = inputs.product
    Mat.fromMutable(d, p, Rational.zero) { m =>
      var r = 0
      cforRange(0 until p) { i =>
        var t = i
        var e = 0
        cforRange(0 until n) { j =>
          val o = inputs(j)
          if (t % o != 0)
            e += 1
          t = (t - (t % o)) / o
        }
        if (e <= 1) {
          m(r, i) := 1
          r += 1
        }
      }
    }
  }

  lazy val matTfromNC: Mat[Rational] = matNCfromT.t

  lazy val matSGfromNG: Mat[Rational] = matNGfromSG.t

  lazy val matNGfromSG: Mat[Rational] = {
    val d = inputs.sum
    val n = inputs.length
    val c = d - n + 1
    Mat.fromMutable(c, d, Rational.zero) { m =>
      cforRange(0 until c) { i =>
        m(i, i) := 1
      }
    }
  }

  lazy val matTfromW: Mat[Rational] = inputs.map(Q(_).matrix).reverse.foldLeft(Mat.ones[Rational](1, 1))(_ kron _)

  lazy val matWfromT: Mat[Rational] = inputs.map(Q(_).matrixInverse).reverse.foldLeft(Mat.ones[Rational](1, 1))(_ kron _)

}

package com.faacets
package core
package text

import cats.data.Validated
import com.faacets.laws.Exprs
import org.scalacheck.Prop
import org.scalatest.Inside

class ExpressionSuite extends FaacetsSuite with Inside {

  val valid =
    Table(
      "scenario" -> "expression",
      "[(2 2) (2 2)]" -> "1/2 <A0> + 1/2",
      "[(2 2) (2 2)]" -> "1/2 * <A0> - 1/2",
      "[(2 2) (3 2)]" -> "P(1,2|1,0)",
      "[(3 2) (2 2)]" -> "P(0,0|0,0) + P(0,0|0,1) + P(0,0|1,0) + P(0,0|1,1)-P(0,1|0,0) + P(0,1|0,1) + P(0,1|1,0) + P(0,1|1,1) -P(1,0|0,0) +      P(1,0|0,1) + P(1,0|1,0) + P(1,0|1,1)-P(1,1|0,0) + P(1,1|0,1) + P(1,1|1,0) + P(1,1|1,1)+P(2,0|0,0) + P(2,0|0,1) -P(2,1|0,0) + P(2,1|0,1)",
      "[(2 2) (2 2)]" -> "P(0,0|0,0) + P(0,0|0,0)",
      "[(2 2) (2 2)]" -> "3P(0,0|0,0) - 3*P(0,0|0,0)",
      "[(2 2) (2 2)]" -> "3/5P(0,0|0,0) - 3/5*P(0,0|0,0)",
      "[(2 2) (2 2) (2 2) (2 2)]" -> "3/5P(0,0,0,0|0,0,0,0) - 3/5*P(0,0,0,0|0,0,0,1)",
      "[(2 2) (3 2)]" -> "PAB(0,1|0,0)",
      "[(2 2) (2 2)]" -> "PAB(0,0|0,0) + PAB(0,0|0,0)",
      "[(2 2) (2 2)]" -> "3PAB(0,0|0,0) - 3*PAB(0,0|0,0)",
      "[(2 2) (2 2)]" -> "3/5PAB(0,0|0,0) - 3/5*PAB(0,0|0,0)",
      "[(2 2) (2 2) (2 2) (2 2)]" -> "3/5PABCD(0,0,0,0|0,0,0,0) - 3/5*PABCD(0,0,0,0|0,0,0,1)",
      "[(2 2) (2 2)]" -> "PA(0|1)",
      "[(2 2) (2 2)]" -> "PB(0|1)",
      "[(2 2) (2 2)]" -> "<A0B0>",
      "[(2 2) (2 2)]" -> "3/2<A0B0>",
      "[(2 2) (2 2)]" -> "3/2*<A0B0> + <A1> -<B1>-5<B0>"
    )

  val invalid =
    Table(
      "scenario" -> "expression",
      "[(2 2) (2 2)]" -> "P(0|0)",
      "[(2 2) (2 2)]" -> "P(1,2|1,1)",
      "[(2 2) (2 2)]" -> "P(00|00)",
      "[(2 2) (2 2)]" -> "P(0 0|0 0)",
      "[(2 2) (2 2)]" -> "P(0,0|00)",
      "[(2 2) (2 2)]" -> "P(0,0,0,0)",
      "[(2 2) (2 2)]" -> "P(0,0|0,0) + +P(0,0|0,0)",
      "[(2 2) (2 2) (2 2)]" -> "3P(0,0|0,0) - 3.0*P(0,0|0,0)",
      "[(2 2) (2 2) (2 2)]" -> "3/5P(0,0|0,0) - 3/5*P(0,0|0,0)",
      "[(2 2) (2 2) (2 2)]" -> "P(0,0|0,0,0)",
      "[(2 2) (2 2) (2 2)]" -> "P(0,0,0|0,0)",
      "[(2 2) (2 2)]" -> "PAB(0|0)",
      "[(2 2) (2 2)]" -> "PAB(1,2|1,1)",
      "[(2 2) (2 2)]" -> "PAB(00|00)",
      "[(2 2) (2 2)]" -> "PAB(0 0|0 0)",
      "[(2 2) (2 2)]" -> "PAB(0,0|00)",
      "[(2 2) (2 2)]" -> "PAB(0,0,0,0)",
      "[(2 2) (2 2)]" -> "PAB(0,0|0,0) + +PAB(0,0|0,0)",
      "[(2 2) (2 2)]" -> "PAB(0,0|0,0) + P(0,0|0,0)",
      "[(2 2) (2 2) (2 2)]" -> "3PAB(0,0|0,0) - 3.0*PAB(0,0|0,0)",
      "[(3 2) (2 2)]" -> "PAB(0,0|0,0) + PAB(0,0|0,1) + PAB(0,0|1,0) + PAB(0,0|1,1)-PAB(0,1|0,0) + PAB(1,0|0,1) + PAB(1,0|0,0) + PAB(0,1|1,1) -PAB(1,0|0,0) +      PAB(1,0|0,1) + PAB(1,0|1,0) + PAB(1,0|1,1)-PAB(1,1|0,0) + PAB(1,1|0,1) + PAB(1,1|1,0) + PAB(1,1|1,1)+PAB(2,0|0,0) + PAB(2,0|0,1) -PAB(2,1|0,0) + PAB(2,1|0,1)",
      "[(2 2) (2 2) (2 2)]" -> "PAB(0,0|0,0,0)",
      "[(2 2) (2 2) (2 2)]" -> "PAB(0,0,0|0,0)",
      "[(2 2) (2 2) (2 2)]" -> "PAB(0,0,0|0,0,0)",
      "[(2 2) (2 2)]" -> "P_AB(1,1|1,1)",
      "[(2 2) (2 2)]" -> "P_A(1,1|1,1)",
      "[(2 2) (2 2)]" -> "P_{AB}(1,1|1,1)",
      "[(2 2) (2 2)]" -> "P^AB(1,1|1,1)",
      "[(2 2) (2 2)]" -> "P^{AB}(1,1|1,1)",
      "[(2 2) (2 2)]" -> "P_A(1|1)",
      "[(2 2) (2 2)]" -> "P^A(1|1)",
      "[(2 2) (2 3)]" -> "<A0B0>",
      "[(2 2) (2 2)]" -> "A0B0",
      "[(2 2) (2 2)]" -> "2A0B0",
      "[(2 2) (2 2)]" -> "2*A0B0"
    )

  test("Valid examples") {
    forAll(valid) { (scenario: String, expr: String) =>
      DExpr.parseExpression(scenario.parseUnsafe[Scenario], expr).fold(_.toList, x => List.empty[String]) shouldBe List.empty[String]
    }
  }

  test("Invalid examples") {
    forAll(invalid) { (scenario: String, expr: String) =>
      DExpr.parseExpression(scenario.parseUnsafe[Scenario], expr).fold(_.toList, x => List.empty[String]) shouldNot be (List.empty[String])
    }
  }

  locally {

    import com.faacets.laws.Scenarios.Small._

    forAll { s: Scenario =>
      val scenario = s
      forAll(Exprs.genExpr(scenario)) { expr: Expr[scenario.type] =>
        inside(Expr.parseExpression(expr.scenario, expr.collinsGisinExpression)) {
          case Validated.Valid(e) => e === expr
          case _ => fail
        }
      }
    }

  }

  locally {

    import com.faacets.laws.Scenarios.BipartiteSmall._

    forAll { s: Scenario =>
      val scenario = s
      forAll(Exprs.genExpr(scenario)) { expr: Expr[scenario.type] =>
        inside(Expr.parseCollinsGisinVector(expr.scenario, expr.collinsGisinTable_BA)) {
          case Validated.Valid(e) => e === expr
          case _ => Prop.falsified
        }
      }
    }

    forAll { s: Scenario =>
      val scenario = s
      forAll(Exprs.genExpr(scenario)) { expr: Expr[scenario.type] =>
        inside(Expr.parseVector(expr.scenario, expr.fullTable_BA)) {
          case Validated.Valid(e) => e === expr
          case _ => Prop.falsified
        }
      }
    }

  }

  locally {

    import com.faacets.laws.Scenarios.BinaryOutputs._

    forAll { s: Scenario =>
      val scenario = s
      forAll(Exprs.genExpr(scenario)) { expr: Expr[scenario.type] =>
        inside(Expr.parseCorrelatorsVector(expr.scenario, expr.correlators.toIndexedSeq.mkString("[", ",", "]"))) {
          case Validated.Valid(e) => e === expr
          case _ => Prop.falsified
        }
      }
    }

    forAll { s: Scenario =>
      val scenario = s
      forAll(Exprs.genExpr(scenario)) { expr: Expr[scenario.type] =>
        inside(Expr.parseExpression(expr.scenario, expr.correlatorsExpression)) {
          case Validated.Valid(e) => e === expr
          case _ => fail
        }
      }
    }
  }

}

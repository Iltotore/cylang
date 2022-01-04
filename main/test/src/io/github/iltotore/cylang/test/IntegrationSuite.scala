package io.github.iltotore.cylang.test

import utest.*
import io.github.iltotore.cylang.{Context, execute}
import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.parse.ExpressionParser.*
import io.github.iltotore.cylang.eval.*
import io.github.iltotore.cylang.eval.given_Evaluator_Expression.evaluate

object IntegrationSuite extends TestSuite {

  val tests: Tests = Tests {

    test("factorial") {

      given Context = Context.empty

      val source: String =
      """PROGRAMME test
        |FONCTION facto(x: entier): entier
        |VARIABLE
        |  res: entier
        |  i: entier
        |DEBUT
        |  res <- 1
        |  POUR i DE 1 A x + 1 FAIRE
        |  res <- res * i
        |  FIN POUR
        |  RETOURNER res
        |FIN
        |
        |VARIABLE
        |  y: entier
        |DEBUT
        |  y <- facto(5)
        |FIN""".stripMargin

      assertMatch(execute(source).map(_._1.scope.variables("y").value)) { case Right(Value.Integer(120)) => }
    }
  }
}

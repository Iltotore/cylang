package io.github.iltotore.cylang.test

import utest.*
import io.github.iltotore.cylang.{Context, execute}
import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.eval.given

import scala.io.Source

object IntegrationSuite extends TestSuite {

  val tests: Tests = Tests {

    given Context = Context.empty

    test("factorial") {

      val source: String =
        """PROGRAMME test
          |FONCTION facto(x: entier): entier
          |VARIABLE
          |  res: entier
          |  i: entier
          |DEBUT
          |  res <- 1
          |  POUR i DE 1 A x + 1 FAIRE
          |    res <- res * i
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

    test("distance") {

      val source: String =
        """PROGRAMME distance
          |
          |STRUCTURE Point
          |  x: entier
          |  y: entier
          |FIN STRUCTURE
          |
          |FONCTION distanceSquared(a: Point, b: Point): entier
          |VARIABLE
          |  distX: entier
          |  distY: entier
          |DEBUT
          |  distX <- b.x-a.x
          |  distY <- b.y-a.y
          |  RETOURNER distX*distX+distY*distY
          |FIN
          |
          |VARIABLE
          |  a: Point
          |  b: Point
          |  result: entier
          |DEBUT
          |  a.x <- 2
          |  a.y <- 3
          |  b.x <- 1
          |  b.y <- 5
          |
          |  result <- distanceSquared(a, b)
          |FIN""".stripMargin

      assertMatch(execute(source).map(_._1.scope.variables("result").value)) { case Right(Value.Integer(5)) => }
    }

    test("stdLib") {

      val source = Source.fromInputStream(getClass.getResourceAsStream("/predef.cy")).mkString

      given Context = execute(source)(using Context.empty).getOrElse(throw new RuntimeException)._1

      def wrap(tpe: String, code: String): String =
        s"""PROGRAMME test
          |
          |VARIABLE
          |  res: $tpe
          |DEBUT
          |  res <- $code
          |FIN""".stripMargin

      test("puissance") - assertMatch(
        execute(wrap("reel", "puissance(2, 4)"))
          .map(_._1.scope.variables("res").value)
      ) { case Right(Value.Number(16)) => }

      test("sqrt") - assertMatch(
        execute(wrap("reel", "sqrt(16)"))
          .map(_._1.scope.variables("res").value)
      ) { case Right(Value.Number(4)) => }

      test("min") {
        test - assertMatch(
          execute(wrap("reel", "min(1, 5)"))
            .map(_._1.scope.variables("res").value)
        ) { case Right(Value.Number(1)) => }

        test - assertMatch(
          execute(wrap("reel", "min(5, 1)"))
            .map(_._1.scope.variables("res").value)
        ) { case Right(Value.Number(1)) => }
      }

      test("max") {
        test - assertMatch(
          execute(wrap("reel", "max(1, 5)"))
            .map(_._1.scope.variables("res").value)
        ) { case Right(Value.Number(5)) => }

        test - assertMatch(
          execute(wrap("reel", "max(5, 1)"))
            .map(_._1.scope.variables("res").value)
        ) { case Right(Value.Number(5)) => }
      }

      test("factorielle") - assertMatch(
        execute(wrap("entier", "factorielle(5)"))
          .map(_._1.scope.variables("res").value)
      ) { case Right(Value.Number(120)) => }

    }
  }
}

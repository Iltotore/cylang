package io.github.iltotore.cylang.test

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.eval.given
import io.github.iltotore.cylang.{Context, execute}
import utest.*

import java.io.{PipedInputStream, PipedOutputStream, PrintStream}
import java.util.Scanner
import scala.io.Source

object JvmIntegrationSuite extends TestSuite {

  val tests: Tests = Tests {

    test("stdLib") {

      val source = Source.fromInputStream(getClass.getResourceAsStream("/predef.cy")).mkString

      given stdCtx: Context = execute(source)(using Context.empty).fold(throw _, _._1)

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

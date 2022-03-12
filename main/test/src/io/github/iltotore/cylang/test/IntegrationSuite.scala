package io.github.iltotore.cylang.test

import utest.*
import io.github.iltotore.cylang.{Context, execute}
import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.eval.given

import java.io.{ByteArrayInputStream, DataInputStream, PipedInputStream, PipedOutputStream, PrintStream}
import java.util.Scanner
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

      given stdCtx: Context = execute(source)(using Context.empty).getOrElse(throw new RuntimeException)._1

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

      test("io") {

        val userOut = new PipedOutputStream()
        val userIn = new PipedInputStream()

        val userScanner = new Scanner(userIn)

        val systemIn = new PipedInputStream(userOut)

        given Context = stdCtx.copy(
          in = systemIn,
          out = new PrintStream(new PipedOutputStream(userIn))
        )

        test("ecrire") {
          assertMatch(execute(
            """PROGRAMME test
              |DEBUT
              |  ECRIRE("out")
              |FIN""".stripMargin
          )) { case Right(_) => }

          assert(userScanner.nextLine() equals "out")
        }

        test("lire") {

          val userPrint = new PrintStream(userOut)

          def testReading(tpe: String, input: String, expected: Value): Unit = {

            userPrint.println(input)

            assertMatch(
              execute(
                s"""PROGRAMME test
                   |
                   |VARIABLE
                   |  x: $tpe
                   |DEBUT
                   |  LIRE(x)
                   |FIN""".stripMargin
              ).map(_._1.scope.variables("x").value)
            ) { case Right(x) if x equals expected => }
          }


          test("boolean") {
            test - testReading("booleen", "true", Value.Bool(true))
            test - testReading("booleen", "false", Value.Bool(false))
          }

          test("integer") - testReading("entier", "1", Value.Integer(1))

          test("real") {
            testReading("reel", "1.0", Value.Real(1.0))
            testReading("reel", "1", Value.Real(1.0))
          }

          test("character") - testReading("caractere", "a", Value.Character('a'))

          test("text") {
            test - testReading("texte", "a", Value.Text("a"))
            test - testReading("texte", "abc", Value.Text("abc"))
          }
        }
      }
    }
  }
}

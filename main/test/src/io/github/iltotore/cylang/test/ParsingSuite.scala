package io.github.iltotore.cylang.test

import io.github.iltotore.cylang.{CYType, Parameter}
import utest.*
import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.parse.ExpressionParser.*

object ParsingSuite extends TestSuite {

  val tests: Tests = Tests {

    test("literal") {

      test("integer") {

        test("positive") - assertMatch(parseAll(integer, "55")) { case Success(Literal(Value.Integer(55)), _) => }
        test("invalid") - assertMatch(parseAll(integer, "a")) { case Failure(_, _) => }
      }

      test("real") {

        test("positive") - assertMatch(parseAll(real, "55.0")) { case Success(Literal(Value.Real(55)), _) => }
        test("invalid") - assertMatch(parseAll(real, "a")) { case Failure(_, _) => }
      }

      test("character") {

        test("valid") - assertMatch(parseAll(character, "'a'")) { case Success(Literal(Value.Character('a')), _) => }
        test("unclosed") {
          test - assertMatch(parseAll(character, "a'")) { case Failure(_, _) => }
          test - assertMatch(parseAll(character, "'a")) { case Failure(_, _) => }
        }
        test("tooLong") - assertMatch(parseAll(character, "'abc'")) { case Failure(_, _) => }
        test("empty") - assertMatch(parseAll(character, "''")) { case Failure(_, _) => }
      }

      test("text") {
        test("valid") {
          test - assertMatch(parseAll(text, "\"a\"")) { case Success(Literal(Value.Text("a")), _) => }
          test - assertMatch(parseAll(text, "\"\"")) { case Success(Literal(Value.Text("")), _) => }
        }
        test("unclosed") {
          test - assertMatch(parseAll(text, "a\"")) { case Failure(_, _) => }
          test - assertMatch(parseAll(text, "\"a")) { case Failure(_, _) => }
        }
      }

      test("bool") {
        test("true") - assertMatch(parseAll(bool, "true")) { case Success(Literal(Value.Bool(true)), _) => }
        test("false") - assertMatch(parseAll(bool, "false")) { case Success(Literal(Value.Bool(false)), _) => }
        test("invalid") - assertMatch(parseAll(bool, "tru")) { case Failure(_, _) => }
      }
    }

    test("operator") {

      test("binary") {

        test("add") {
          test("valid") - assertMatch(parseAll(arith, "1 + 1")) { case Success(Addition(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(arith, "1 +")) { case Failure(_, _) => }
        }

        test("sub") {
          test("valid") - assertMatch(parseAll(arith, "1 - 1")) { case Success(Substraction(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(arith, "1 -")) { case Failure(_, _) => }
        }

        test("mul") {
          test("valid") - assertMatch(parseAll(term, "1 * 1")) { case Success(Multiplication(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(term, "1 *")) { case Failure(_, _) => }
        }

        test("div") {
          test("valid") - assertMatch(parseAll(term, "1 / 1")) { case Success(Division(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(term, "1 /")) { case Failure(_, _) => }
        }

        test("wholeDiv") {
          test("valid") - assertMatch(parseAll(term, "1 DIV 1")) { case Success(WholeDivision(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(term, "1 DIV")) { case Failure(_, _) => }
        }

        test("modulo") {
          test("valid") - assertMatch(parseAll(term, "1 % 1")) { case Success(Modulo(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(term, "1 /")) { case Failure(_, _) => }
        }

        test("priority") {

          test("parantheseless") {

            val expected = Equality(
              Literal(Value.Integer(11)),
              Addition(
                Literal(Value.Integer(5)),
                Multiplication(
                  Literal(Value.Integer(2)),
                  Literal(Value.Integer(3))
                )
              )
            )

            assertMatch(parseAll(equality, "11 = 5 + 2 * 3")) { case Success(parsed, _) if parsed equals expected => }
          }

          test("paranthesized") {

            val expected = Equality(
              Literal(Value.Integer(21)),
              Multiplication(
                Addition(
                  Literal(Value.Integer(5)),
                  Literal(Value.Integer(2))
                ),
                Literal(Value.Integer(3))
              )
            )

            assertMatch(parseAll(equality, "21 = (5 + 2) * 3")) { case Success(parsed, _) if parsed equals expected => }
          }
        }
      }

      test("unary") {

        test("plus") - assertMatch(parseAll(unary, "+5")) { case Success(Literal(Value.Integer(5))) => }

        test("minus") - assertMatch(parseAll(unary, "-5")) { case Success(Negation(Literal(Value.Integer(5)))) => }

        test("not") - assertMatch(parseAll(unary, "!true")) { case Success(Not(Literal(Value.Bool(true)))) => }
      }
    }

    test("tree") {
      assertMatch(parseAll(
        tree("FIN TEST"),
        """1 + 1
          |5 * 2
          |FIN TEST""".stripMargin
      )) { case Success(Tree(List(_, _))) => }
    }

    test("forLoop") {
      test("simple") - assertMatch(parseAll(
        forLoop,
        """POUR i DE 0 A 5 FAIRE
          |ecrire(i)
          |FIN POUR""".stripMargin
      )) { case Success(ForLoop("i", Literal(_), Literal(_), Literal(Value.Integer(1)), Tree(List(_)))) => }

      test("withStep") - assertMatch(parseAll(
        forLoop,
        """POUR i DE 0 A 5 PAS DE 2 FAIRE
          |ecrire(i)
          |FIN POUR""".stripMargin
      )) { case Success(ForLoop("i", Literal(_), Literal(_), Literal(Value.Integer(2)), Tree(List(_)))) => }
    }

    test("whileLoop") {
      assertMatch(parseAll(
        whileLoop,
        """TANT QUE true FAIRE
          |ecrire(1)
          |FIN TANT QUE""".stripMargin
      )) { case Success(WhileLoop(Literal(_), Tree(List(_)))) => }
    }

    test("if") {
      test("simple") - assertMatch(parseAll(
        ifElse,
        """SI true FAIRE
          |ecrire(1)
          |FIN SI""".stripMargin
      )) { case Success(If(Literal(_), Tree(List(_)), Empty)) => }

      test("withElse") - assertMatch(parseAll(
        ifElse,
        """SI true FAIRE
          |ecrire(1)
          |SINON FAIRE
          |ecrire(2)
          |FIN SI""".stripMargin
      )) { case Success(If(Literal(_), Tree(List(_)), Tree(List(_)))) => }

      test("withElseIf") - assertMatch(parseAll(
        ifElse,
        """SI true FAIRE
          |ecrire(1)
          |SINON SI true FAIRE
          |ecrire(2)
          |SINON FAIRE
          |ecrire(3)
          |FIN SI""".stripMargin
      )) { case Success(If(Literal(_), Tree(List(_)), If(Literal(_), Tree(List(_)), Tree(List(_))))) => }
    }

    test("return") - assertMatch(parseAll(treeReturn, "RETOURNER 5")) { case Success(Return(Literal(Value.Integer(5)))) => }

    test("functionCall") {
      test("empty") - assertMatch(parseAll(expression, "foo()")) { case Success(FunctionCall("foo", List())) => }
      test("withArgs") - assertMatch(parseAll(expression, "max(1, 2)")) { case Success(FunctionCall("max", List(_, _))) => }
    }

    test("cyType") {
      test("valid") - assertMatch(parseAll(cyType, "entier")) { case Success(CYType.Integer) => }
      test("unknown") - assertMatch(parseAll(cyType, "foo")) { case Failure(_, _) => }
    }

    test("param") - assertMatch(parseAll(param, "x: entier")) { case Success(Parameter("x", CYType.Integer)) => }

    test("body") {
      test("simple") - assertMatch(parseAll(
        body,
        """DEBUT
          |ecrire(1)
          |FIN""".stripMargin
      )) { case Success(Body(List(), _)) =>}

      test("withVariables") - assertMatch(parseAll(
        body,
        """VARIABLE
          |x: entier
          |y: caractere
          |DEBUT
          |POUR i DE 0 A x FAIRE
          |ecrire(x + i)
          |FIN POUR
          |FIN""".stripMargin
      )) { case Success(Body(List(_, _), _)) =>}
    }

    test("functionDeclaration") - assertMatch(parseAll(
      functionDeclaration,
      """FONCTION facto(x: entier): entier
        |VARIABLE
        |res: entier
        |DEBUT
        |res <- 1
        |POUR i DE 0 A x FAIRE
        |res = res + i
        |FIN POUR
        |RETOURNER res
        |FIN""".stripMargin
    )) { case Success(FunctionDeclaration("facto", CYType.Integer, _, _)) => }

    test("program") {

      val expected = ProgramDeclaration(
        name = "test",
        functions = List(
          FunctionDeclaration(
            name = "facto",
            tpe = CYType.Integer,
            parameters = List(Parameter("x", CYType.Integer)),
            body = Body(
              variables = List(Parameter("res", CYType.Integer)),
              expression = Tree(List(
                VariableAssignment(
                  name = "res",
                  expression = Literal(Value.Integer(1))
                ),
                ForLoop(
                  name = "i",
                  from = Literal(Value.Integer(1)),
                  to = Addition(VariableCall("x"), Literal(Value.Integer(1))),
                  step = Literal(Value.Integer(1)),
                  expression = Tree(List(
                    VariableAssignment(
                      name = "res",
                      expression = Multiplication(VariableCall("res"), VariableCall("i"))
                    )
                  ))
                ),
                Return(VariableCall("res"))
              ))
            )
          )
        ),
        body = Body(
          variables = List.empty,
          expression = Tree(List(
            FunctionCall("ecrire", List(
              FunctionCall("facto", List(Literal(Value.Integer(5)))))
            )
          ))
        )
      )

      assertMatch(parseAll(
        program,
        """PROGRAMME test
          |FONCTION facto(x: entier): entier
          |VARIABLE
          |res: entier
          |DEBUT
          |res <- 1
          |POUR i DE 1 A x + 1 FAIRE
          |res <- res * i
          |FIN POUR
          |RETOURNER res
          |FIN
          |
          |DEBUT
          |ecrire(facto(5))
          |FIN""".stripMargin
      )) { case Success(result) if result equals expected => }
    }
  }
}

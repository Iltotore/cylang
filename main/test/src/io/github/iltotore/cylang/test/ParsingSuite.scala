package io.github.iltotore.cylang.test

import utest.*

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.parse.ExpressionParser.*

object ParsingSuite extends TestSuite {

  val tests: Tests = Tests {

    test("literal") {

      test("integer") {

        test("positive") - assertMatch(parseAll(expression, "55")) { case Success(Literal(Value.Integer(55)), _) => }
        test("negative") - assertMatch(parseAll(expression, "-55")) { case Success(Literal(Value.Integer(-55)), _) => }
        test("invalid") - assertMatch(parseAll(expression, "a")) { case Success(VariableCall("a"), _) => }
      }

      test("real") {

        test("positive") - assertMatch(parseAll(expression, "55.0")) { case Success(Literal(Value.Real(55)), _) => }
        test("negative") - assertMatch(parseAll(expression, "-55.0")) { case Success(Literal(Value.Real(-55)), _) => }
        test("invalid") - assertMatch(parseAll(expression, "a")) { case Success(VariableCall("a"), _) => }
      }

      test("character") {

        test("valid") - assertMatch(parseAll(expression, "'a'")) { case Success(Literal(Value.Character('a')), _) => }
        test("unclosed") {
          test - assertMatch(parseAll(expression, "a'")) { case Failure(_, _) => }
          test - assertMatch(parseAll(expression, "'a")) { case Failure(_, _) => }
        }
        test("tooLong") - assertMatch(parseAll(expression, "'abc'")) { case Failure(_, _) => }
        test("empty") - assertMatch(parseAll(expression, "''")) { case Failure(_, _) => }
      }

      test("text") {
        test("valid") {
          test - assertMatch(parseAll(expression, "\"a\"")) { case Success(Literal(Value.Text("a")), _) => }
          test - assertMatch(parseAll(expression, "\"\"")) { case Success(Literal(Value.Text("")), _) => }
        }
        test("unclosed") {
          test - assertMatch(parseAll(expression, "a\"")) { case Failure(_, _) => }
          test - assertMatch(parseAll(expression, "\"a")) { case Failure(_, _) => }
        }
      }

      test("bool") {
        test("true") - assertMatch(parseAll(expression, "true")) { case Success(Literal(Value.Bool(true)), _) => }
        test("false") - assertMatch(parseAll(expression, "false")) { case Success(Literal(Value.Bool(false)), _) => }
        test("invalid") - assertMatch(parseAll(expression, "tru")) { case Success(VariableCall("tru"), _) => }
      }
    }

    test("operator") {

      test("binary") {

        test("add") {
          test("valid") - assertMatch(parseAll(expression, "1 + 1")) { case Success(Addition(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(expression, "1 +")) { case Failure(_, _) => }
        }

        test("sub") {
          test("valid") - assertMatch(parseAll(expression, "1 - 1")) { case Success(Substraction(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(expression, "1 -")) { case Failure(_, _) => }
        }

        test("mul") {
          test("valid") - assertMatch(parseAll(expression, "1 * 1")) { case Success(Multiplication(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(expression, "1 *")) { case Failure(_, _) => }
        }

        test("div") {
          test("valid") - assertMatch(parseAll(expression, "1 / 1")) { case Success(Division(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(expression, "1 /")) { case Failure(_, _) => }
        }

        test("modulo") {
          test("valid") - assertMatch(parseAll(expression, "1 % 1")) { case Success(Modulo(_, _), _) => }
          test("unclosed") - assertMatch(parseAll(expression, "1 /")) { case Failure(_, _) => }
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

            assertMatch(parseAll(expression, "11 = 5 + 2 * 3")) { case Success(parsed, _) if parsed equals expected => }
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

            assertMatch(parseAll(expression, "21 = (5 + 2) * 3")) { case Success(parsed, _) if parsed equals expected => }
          }
        }
      }
    }
  }
}

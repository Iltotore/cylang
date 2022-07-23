package io.github.iltotore.cylang.test

import utest.*

import scala.util.parsing.input.{Position, NoPosition}
import io.github.iltotore.cylang.{CYType, FixedPosition, Parameter}
import io.github.iltotore.cylang.ast.{Body, Expression, Value}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.parse.Token
import io.github.iltotore.cylang.parse.Token.*
import io.github.iltotore.cylang.parse.TokenReader
import io.github.iltotore.cylang.parse.ExpressionParser.*

object ParsingSuite extends TestSuite {

  private def assertSuccess[A](parser: Parser[A], tokens: List[Token], expected: A): Unit = {
    val result = parser(TokenReader(tokens))
    Predef.assert(result.successful, s"Result is not successful: $result")
    Predef.assert(result.next.atEnd, s"Remaining input: ${result.next}")
    assert(result.get == expected)
  }

  private def assertFailure[A](parser: Parser[A], tokens: List[Token]): Unit = assertMatch(parser(TokenReader(tokens))) {
    case Success(_, in) if !in.atEnd =>
    case NoSuccess(_, _) =>
  }

  private def assertLiteral(parser: Parser[Expression], token: Token, expected: Value)(using Position): Unit = assertSuccess(parser, List(token), Literal(expected))

  val tests: Tests = Tests {

    given Position = NoPosition

    test("literal") {
      test("bool") - assertLiteral(bool, LiteralBool(true), Value.Bool(true))
      test("int") - assertLiteral(integer, LiteralInt(14), Value.Integer(14))
      test("real") - assertLiteral(real, LiteralReal(14.0), Value.Real(14.0))
      test("char") - assertLiteral(character, LiteralChar('a'), Value.Character('a'))
      test("text") - assertLiteral(text, LiteralText("abc"), Value.Text("abc"))
    }

    test("operator") {

      test("unary") {
        test("+") - assertSuccess(unary, List(Operator("+"), LiteralInt(14)), Literal(Value.Integer(14)))
        test("-") - assertSuccess(unary, List(Operator("-"), LiteralInt(14)), Negation(Literal(Value.Integer(14))))
        test("!") - assertSuccess(unary, List(Operator("!"), LiteralBool(true)), Not(Literal(Value.Bool(true))))
      }

      test("binary") {

        test("eq") {
          test("valid") - assertSuccess(comparison, List(LiteralInt(1), Operator("="), LiteralInt(2)), Equality(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(comparison, List(LiteralInt(1), Operator("=")))
        }

        test("lt") {
          test("valid") - assertSuccess(comparison, List(LiteralInt(1), Operator("<"), LiteralInt(2)), Less(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(comparison, List(LiteralInt(1), Operator("<")))
        }

        test("gt") {
          test("valid") - assertSuccess(comparison, List(LiteralInt(1), Operator(">"), LiteralInt(2)), Greater(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(comparison, List(LiteralInt(1), Operator(">")))
        }

        test("lteq") {
          test("valid") - assertSuccess(comparison, List(LiteralInt(1), Operator("<="), LiteralInt(2)), LessEqual(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(comparison, List(LiteralInt(1), Operator("<=")))
        }

        test("gteq") {
          test("valid") - assertSuccess(comparison, List(LiteralInt(1), Operator(">="), LiteralInt(2)), GreaterEqual(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(comparison, List(LiteralInt(1), Operator(">=")))
        }

        test("add") {
          test("valid") - assertSuccess(arith, List(LiteralInt(1), Operator("+"), LiteralInt(2)), Addition(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(arith, List(LiteralInt(1), Operator("+")))
        }

        test("sub") {
          test("valid") - assertSuccess(arith, List(LiteralInt(1), Operator("-"), LiteralInt(2)), Subtraction(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(arith, List(LiteralInt(1), Operator("-")))
        }

        test("mul") {
          test("valid") - assertSuccess(term, List(LiteralInt(1), Operator("*"), LiteralInt(2)), Multiplication(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(term, List(LiteralInt(1), Operator("*")))
        }

        test("div") {
          test("valid") - assertSuccess(term, List(LiteralInt(1), Operator("/"), LiteralInt(2)), Division(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(term, List(LiteralInt(1), Operator("/")))
        }

        test("wholeDiv") {
          test("valid") - assertSuccess(term, List(LiteralInt(1), Operator("DIV"), LiteralInt(2)), WholeDivision(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(term, List(LiteralInt(1), Operator("DIV")))
        }

        test("mod") {
          test("valid") - assertSuccess(term, List(LiteralInt(1), Operator("MOD"), LiteralInt(2)), Modulo(Literal(Value.Integer(1)), Literal(Value.Integer(2))))
          test("missing") - assertFailure(term, List(LiteralInt(1), Operator("MOD")))
        }

        test("priority") {
          test("parenthesisLess") {

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

            assertSuccess(expression, List(LiteralInt(11), Operator("="), LiteralInt(5), Operator("+"), LiteralInt(2), Operator("*"), LiteralInt(3)), expected)
          }

          test("withParentheses") {

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

            assertSuccess(expression, List(LiteralInt(21), Operator("="), ParenthesisOpen(), LiteralInt(5), Operator("+"), LiteralInt(2), ParenthesisClose(), Operator("*"), LiteralInt(3)), expected)
          }
        }

      }
    }

    test("tree") {

      val expected = Tree(List(
        Addition(
          Literal(Value.Integer(1)),
          Literal(Value.Integer(1))
        )
      ))

      assertSuccess(tree(End()), List(LiteralInt(1), Operator("+"), LiteralInt(1), End()), expected)
    }

    test("forLoop") {

      test("basic") {

        val expected = ForLoop(
          "i",
          Literal(Value.Integer(0)),
          Literal(Value.Integer(10)),
          Literal(Value.Integer(1)),
          Tree(List.empty)
        )

        assertSuccess(forLoop, List(For(), Identifier("i"), From(), LiteralInt(0), To(), LiteralInt(10), Do(), End(), For()), expected)
      }

      test("withStep") {

        val expected = ForLoop(
          "i",
          Literal(Value.Integer(0)),
          Literal(Value.Integer(10)),
          Literal(Value.Integer(2)),
          Tree(List.empty)
        )

        assertSuccess(forLoop, List(For(), Identifier("i"), From(), LiteralInt(0), To(), LiteralInt(10), Step(), LiteralInt(2), Do(), End(), For()), expected)
      }
    }

    test("whileLoop") {

      val expected = WhileLoop(
        Literal(Value.Bool(true)),
        Tree(List.empty)
      )

      assertSuccess(whileLoop, List(While(), LiteralBool(true), Do(), End(), While()), expected)
    }

    test("doWhileLoop") {

      val expected = DoWhileLoop(
        Literal(Value.Bool(true)),
        Tree(List.empty)
      )

      assertSuccess(doWhileLoop, List(Do(), While(), LiteralBool(true)), expected)
    }

    test("if") {

      test("basic") {

        val expected = IfCondition(
          Literal(Value.Bool(true)),
          Tree(List.empty),
          Empty()
        )

        assertSuccess(ifElse, List(If(), LiteralBool(true), Then(), End(), If()), expected)
      }

      test("withElse") {

        val expected = IfCondition(
          Literal(Value.Bool(true)),
          Tree(List.empty),
          Tree(List.empty)
        )

        assertSuccess(ifElse, List(If(), LiteralBool(true), Then(), Else(), End(), If()), expected)
      }

      test("withElseIf") {

        val expected = IfCondition(
          Literal(Value.Bool(true)),
          Tree(List.empty),
          IfCondition(
            Literal(Value.Bool(true)),
            Tree(List.empty),
            Tree(List.empty)
          )
        )

        assertSuccess(ifElse, List(If(), LiteralBool(true), Then(), Else(), If(), LiteralBool(true), Then(), Else(), End(), If()), expected)
      }
    }

    test("return") {
      test("void") - assertSuccess(treeReturn, List(Return()), ReturnExpr(Empty()))
      test("value") - assertSuccess(treeReturn, List(Return(), LiteralInt(1)), ReturnExpr(Literal(Value.Integer(1))))
    }

    test("variableCall") - assertSuccess(variableCall, List(Identifier("var")), VariableCall("var"))

    test("functionCall") {
      test("empty") - assertSuccess(functionCall, List(Identifier("test"), ParenthesisOpen(), ParenthesisClose()), FunctionCall("test", List.empty))
      test("withArgs") - assertSuccess(functionCall, List(Identifier("test"), ParenthesisOpen(), LiteralInt(1), ParenthesisClose()), FunctionCall("test", List(Literal(Value.Integer(1)))))
    }

    test("arrayCall") - assertSuccess(unary, List(Identifier("arr"), BracketOpen(), LiteralInt(1), BracketClose()), ArrayCall(VariableCall("arr"), Literal(Value.Integer(1))))

    test("structureCall") - assertSuccess(unary, List(Identifier("struct"), Dot(), Identifier("x")), StructureCall(VariableCall("struct"), "x"))

    test("variableAssignment") - assertSuccess(variableAssignment, List(Identifier("var"), Assignment(), LiteralInt(1)), VariableAssignment("var", Literal(Value.Integer(1))))

    test("arrayAssignment") - assertSuccess(arrayAssignment, List(Identifier("arr"), BracketOpen(), LiteralInt(1), BracketClose(), Assignment(), LiteralInt(3)), ArrayAssignment(VariableCall("arr"), Literal(Value.Integer(1)), Literal(Value.Integer(3))))

    test("structureAssignment") - assertSuccess(structureAssignment, List(Identifier("struct"), Dot(), Identifier("x"), Assignment(), LiteralInt(1)), StructureAssignment(VariableCall("struct"), "x", Literal(Value.Integer(1))))

    test("type") {
      test("basic") - assertSuccess(cyType, List(Identifier("entier")), CYType.Integer)
      test("array") {
        test("unknownSize") - assertSuccess(cyType, List(ArrayOf(), Identifier("entier")), CYType.Array(CYType.Integer, None))
        test("knownSize") {
          test - assertSuccess(cyType, List(ArrayOf(), Identifier("entier"), ArraySize(), LiteralInt(10)), CYType.Array(CYType.Integer, Some(10)))
          test - assertFailure(cyType, List(ArrayOf(), Identifier("entier"), ArraySize(), LiteralReal(10.0)))
        }
      }
      test("structure") - assertSuccess(cyType, List(Identifier("Point")), CYType.StructureInstance("Point"))
    }

    test("constantDeclaration") - assertSuccess(constantDeclaration, List(Constant(), Identifier("x"), Assignment(), LiteralInt(1)), ConstantDeclaration("x", Literal(Value.Integer(1))))

    test("enumerationDeclaration") - assertSuccess(enumerationDeclaration, List(Enumeration(), Identifier("Couleur"), Identifier("BLEU"), End(), Enumeration()), EnumerationDeclaration("Couleur", List("BLEU")))

    test("structureDeclaration") {

      val expected = StructureDeclaration(
        "Point",
        List(
          Parameter("x", CYType.Real),
          Parameter("y", CYType.Real)
        )
      )

      assertSuccess(structureDeclaration, List(Structure(), Identifier("Point"), Identifier("x"), Colon(), Identifier("reel"), Identifier("y"), Colon(), Identifier("reel"), End(), Structure()), expected)
    }

    test("functionDeclaration") {

      val expected = FunctionDeclaration(
        "test",
        CYType.Boolean,
        List(Parameter("x", CYType.Integer)),
        Body(List.empty, Tree(List(ReturnExpr(Literal(Value.Bool(true))))))
      )

      assertSuccess(functionDeclaration, List(Function(), Identifier("test"), ParenthesisOpen(), Identifier("x"), Colon(), Identifier("entier"), ParenthesisClose(), Colon(), Identifier("booleen"), Begin(), Return(), LiteralBool(true), End()), expected)
    }

    test("procedureDeclaration") {

      val expected = FunctionDeclaration(
        "test",
        CYType.Void,
        List(Parameter("x", CYType.Integer)),
        Body(List.empty, Tree(List.empty))
      )

      assertSuccess(procedureDeclaration, List(Procedure(), Identifier("test"), ParenthesisOpen(), Identifier("x"), Colon(), Identifier("entier"), ParenthesisClose(), Begin(), End()), expected)
    }
  }
}

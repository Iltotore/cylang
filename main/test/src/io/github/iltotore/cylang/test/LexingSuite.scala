package io.github.iltotore.cylang.test

import utest.*
import io.github.iltotore.cylang.parse.ExpressionLexer.apply as lex
import io.github.iltotore.cylang.parse.Token
import io.github.iltotore.cylang.parse.Token.*

object LexingSuite extends TestSuite {

  private def assertSingle(code: String, token: Token) = assertMatch(lex(code)) { case Right(List(t)) if t equals token => }

  val tests: Tests = Tests {

    test("symbol") {
      test("comma") - assertSingle(",", Comma)
      test("dot") - assertSingle(".", Dot)
      test("colon") - assertSingle(":", Colon)
      test("parenthesisOpen") - assertSingle("(", ParenthesisOpen)
      test("parenthesisClose") - assertSingle(")", ParenthesisClose)
      test("bracketOpen") - assertSingle("[", BracketOpen)
      test("bracketClose") - assertSingle("]", BracketClose)
      test("assignment") - assertSingle("<-", Assignment)
    }

    test("keyword") {
      test("program") - assertSingle("PROGRAMME", Program)
      test("begin") - assertSingle("DEBUT", Begin)
      test("end") - assertSingle("FIN", End)
      test("variable") - assertSingle("VARIABLE", Variable)
      test("function") - assertSingle("FONCTION", Function)
      test("procedure") - assertSingle("PROCEDURE", Procedure)
      test("constant") - assertSingle("CONSTANTE", Constant)
      test("structure") - assertSingle("STRUCTURE", Structure)
      test("enumeration") - assertSingle("ENUMERATION", Enumeration)
      test("if") - assertSingle("SI", If)
      test("then") - assertSingle("ALORS", Then)
      test("else") - assertSingle("SINON", Else)
      test("and") - assertSingle("ET", And)
      test("or") - assertSingle("OU", Or)
      test("for") - assertSingle("POUR", For)
      test("from") - assertSingle("DE", From)
      test("to") - assertSingle("A", To)
      test("step") - assertSingle("PAS DE", Step)
      test("while") - assertSingle("TANT QUE", While)
      test("do") - assertSingle("FAIRE", Do)
      test("return") - assertSingle("RETOURNER", Return)
      test("arrayOf") - assertSingle("tableau de", ArrayOf)
      test("arraySize") - assertSingle("de taille", ArraySize)
    }

    test("misc") {
      test("literalBool") {
        test("true") - assertSingle("vrai", LiteralBool(true))
        test("false") - assertSingle("faux", LiteralBool(false))
      }
      test("literalInt") - assertSingle("14", LiteralInt(14))
      test("literalReal") - assertSingle("14.0", LiteralReal(14.0))
      test("literalChar") - assertSingle("'a'", LiteralChar('a'))
      test("literalText") {
        test("empty") - assertSingle("\"\"", LiteralText(""))
        test("nonEmpty") - assertSingle("\"abc\"", LiteralText("abc"))
      }
      test("identifier") - assertSingle("abc", Identifier("abc"))
      test("operator") {
        test("+") - assertSingle("+", Operator("+"))
        test("-") - assertSingle("-", Operator("-"))
        test("*") - assertSingle("*", Operator("*"))
        test("/") - assertSingle("/", Operator("/"))
        test("!") - assertSingle("!", Operator("!"))
        test("DIV") - assertSingle("DIV", Operator("DIV"))
        test("MOD") - assertSingle("MOD", Operator("MOD"))
      }
    }
  }
}

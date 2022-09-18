package io.github.iltotore.cylang.test

import io.github.iltotore.cylang.parse.ExpressionLexer.apply as lex
import io.github.iltotore.cylang.parse.{ExpressionLexer, Token}
import io.github.iltotore.cylang.parse.Token.*
import utest.*

object LexingSuite extends TestSuite {

  val tests: Tests = Tests {

    test("symbol") {
      test("comma") - assertLex(",", Comma())
      test("dot") - assertLex(".", Dot())
      test("colon") - assertLex(":", Colon())
      test("parenthesisOpen") - assertLex("(", ParenthesisOpen())
      test("parenthesisClose") - assertLex(")", ParenthesisClose())
      test("bracketOpen") - assertLex("[", BracketOpen())
      test("bracketClose") - assertLex("]", BracketClose())
      test("assignment") - assertLex("<-", Assignment())
    }

    test("keyword") {
      test("program") - assertLex("PROGRAMME", Program())
      test("begin") - assertLex("DEBUT", Begin())
      test("end") - assertLex("FIN", End())
      test("variable") - assertLex("VARIABLE", Variable())
      test("function") - assertLex("FONCTION", Function())
      test("procedure") - assertLex("PROCEDURE", Procedure())
      test("constant") - assertLex("CONSTANTE", Constant())
      test("structure") - assertLex("STRUCTURE", Structure())
      test("enumeration") - assertLex("ENUMERATION", Enumeration())
      test("if") - assertLex("SI", If())
      test("then") - assertLex("ALORS", Then())
      test("else") - assertLex("SINON", Else())
      test("for") - assertLex("POUR", For())
      test("from") - assertLex("DE", From())
      test("to") - assertLex("A", To())
      test("step") - assertLex("PAS DE", Step())
      test("while") - assertLex("TANT QUE", While())
      test("do") - assertLex("FAIRE", Do())
      test("return") - assertLex("RETOURNER", Return())
      test("arrayOf") - assertLex("tableau de type", ArrayOf())
      test("arraySize") - assertLex("de taille", ArraySize())
    }

    test("misc") {
      test("literalBool") {
        test("true") - assertLex("vrai", LiteralBool(true))
        test("false") - assertLex("faux", LiteralBool(false))
      }
      test("literalInt") - assertLex("14", LiteralInt(14))
      test("literalReal") - assertLex("14.0", LiteralReal(14.0))
      test("literalChar") - assertLex("'a'", LiteralChar('a'))
      test("literalText") {
        test("empty") - assertLex("\"\"", LiteralText(""))
        test("nonEmpty") - assertLex("\"abc\"", LiteralText("abc"))
      }
      test("identifier") - assertLex("abc", Identifier("abc"))
      test("operator") {
        test("+") - assertLex("+", Operator("+"))
        test("-") - assertLex("-", Operator("-"))
        test("*") - assertLex("*", Operator("*"))
        test("/") - assertLex("/", Operator("/"))
        test("!") - assertLex("!", Operator("!"))
        test("DIV") - assertLex("DIV", Operator("DIV"))
        test("MOD") - assertLex("MOD", Operator("MOD"))
        test("OU") - assertLex("OU", Operator("OU"))
        test("ET") - assertLex("ET", Operator("ET"))
      }
      test("comment") {
        test - assertLex("//blabla")
        test - assertLex("5 //hey", LiteralInt(5))
        test - assertLex(
          """5 //hey
            |3 //hello""".stripMargin,
          LiteralInt(5), LiteralInt(3)
        )
      }
    }
  }

  private def assertLex(code: String, tokens: Token*): Unit = assert(lex(code) == Right(tokens.toList))
}

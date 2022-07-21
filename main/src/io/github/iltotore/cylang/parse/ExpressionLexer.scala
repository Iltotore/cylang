package io.github.iltotore.cylang.parse

import scala.util.parsing.combinator.RegexParsers
import Token.*

import scala.language.implicitConversions
import scala.util.matching.Regex

//noinspection TypeAnnotation
object ExpressionLexer extends RegexParsers {

  /** A parser that matches a regex string without handling spaces */
  def regexNoSkip(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = offset
      r.findPrefixMatchOf(new SubSequence(source, start)) match {
        case Some(matched) =>
          Success(source.subSequence(start, start + matched.end).toString,
            in.drop(start + matched.end - offset))
        case None =>
          val found = if (start == source.length()) "end of source" else "'"+source.charAt(start)+"'"
          Failure("string matching regex '"+r+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }

  def keywordEnd = regexNoSkip((s"($$|$whiteSpace)").r)
    .withErrorMessage("Whitespace or end of source expected")
    .withFailureMessage("Whitespace or end of source expected")

  def symbol(word: Parser[String], token: Token): Parser[Token] = positioned(word ^^^ token)

  def keyword(word: Parser[String], token: Token): Parser[Token] = positioned(word ~>! keywordEnd ^^^ token)

  def comma = symbol(",", Comma())

  def dot = symbol(".", Dot())

  def colon = symbol(":", Colon())

  def parenthesisOpen = symbol("(", ParenthesisOpen())

  def parenthesisClose = symbol(")", ParenthesisClose())

  def bracketOpen = symbol("[", BracketOpen())

  def bracketClose = symbol("]", BracketClose())

  def assignment = symbol("<-", Assignment())

  def program = keyword("PROGRAMME", Program())

  def begin = keyword("DEBUT", Begin())

  def end = keyword("FIN", End())

  def variable = keyword("VARIABLE", Variable())

  def function = keyword("FONCTION", Function())

  def procedure = keyword("PROCEDURE", Procedure())

  def constant = keyword("CONSTANTE", Constant())

  def structure = keyword("STRUCTURE", Structure())

  def enumeration = keyword("ENUMERATION", Enumeration())

  def ifCond = keyword("SI", If())

  def thenCond = keyword("ALORS", Then())

  def elseCond = keyword("SINON", Else())

  def and = keyword("ET", And())

  def or = keyword("OU", Or())

  def forLoop = keyword("POUR", For())

  def from = keyword("DE", From())

  def to = keyword("A", To())

  def step = keyword("PAS" ~>! "DE", Step())

  def whileLoop = keyword("TANT" ~>! "QUE", While())

  def doToken = keyword("FAIRE", Do())

  def returnToken = keyword("RETOURNER", Return())

  def arrayOf = keyword("tableau" ~>! "de", ArrayOf())

  def arraySize = keyword("de" ~>! "taille", ArraySize())

  def literalBool = positioned("(vrai)|(faux)".r ^^ (x => LiteralBool(x equals "vrai")))

  def literalInt = positioned(raw"\d+".r ^^ (x => LiteralInt(x.toInt)))

  def literalReal = positioned(raw"\d+\.\d+".r ^^ (x => LiteralReal(x.toDouble)))

  def literalChar = positioned("'[^']'".r ^^ (x => LiteralChar(x.charAt(1))))

  def literalText = positioned("\"[^\"]*\"".r ^^ (x => LiteralText(x.substring(1, x.length-1))))

  def operator = positioned(raw"[+\-*/!]|(DIV)|(MOD)|([<>]=?)|(!?=)".r ^^ Operator.apply)

  def identifier = positioned(raw"\w+".r ^^ Identifier.apply)

  def tokens: Parser[List[Token]] = rep1(
    comma | dot | colon | parenthesisOpen | parenthesisClose | bracketOpen | bracketClose | assignment | program | begin
      | end | variable | function | procedure | constant | structure | enumeration | elseCond | ifCond | thenCond | and
      | or | forLoop | from | to | step | whileLoop | doToken | returnToken | arrayOf | arraySize | literalBool
      | literalReal | literalInt | literalChar | literalText | operator | identifier
  )

  def apply(code: String): Either[ParsingError, List[Token]] = parseAll(tokens, code) match {
    case Success(result, _) => Right(result)
    case failure: Failure => Left(ParsingError(failure.toString))
    case err: Error => Left(ParsingError(err.toString))
  }
}

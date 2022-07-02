package io.github.iltotore.cylang.parse

import scala.util.parsing.combinator.RegexParsers
import Token.*

//noinspection TypeAnnotation
object ExpressionLexer extends RegexParsers {

  def symbol(word: String, token: Token): Parser[Token] = word ^^ (_ => token)

  def keyword(word: String, token: Token): Parser[Token] = (s"$word($$|$whiteSpace)").r ^^ (_ => token)

  def comma = symbol(",", Comma)

  def dot = symbol(".", Dot)

  def colon = symbol(":", Colon)

  def parenthesisOpen = symbol("(", ParenthesisOpen)

  def parenthesisClose = symbol(")", ParenthesisClose)

  def bracketOpen = symbol("[", BracketOpen)

  def bracketClose = symbol("]", BracketClose)

  def assignment = symbol("<-", Assignment)

  def program = keyword("PROGRAMME", Program)

  def begin = keyword("DEBUT", Begin)

  def end = keyword("FIN", End)

  def variable = keyword("VARIABLE", Variable)

  def function = keyword("FONCTION", Function)

  def procedure = keyword("PROCEDURE", Procedure)

  def constant = keyword("CONSTANTE", Constant)

  def structure = keyword("STRUCTURE", Structure)

  def enumeration = keyword("ENUMERATION", Enumeration)

  def ifCond = keyword("SI", If)

  def thenCond = keyword("ALORS", Then)

  def elseCond = keyword("SINON", Else)

  def and = keyword("ET", And)

  def or = keyword("OU", Or)

  def forLoop = keyword("POUR", For)

  def from = keyword("DE", From)

  def to = keyword("A", To)

  def step = keyword("PAS DE", Step)

  def whileLoop = keyword("TANT QUE", While)

  def doToken = keyword("FAIRE", Do)

  def returnToken = keyword("RETOURNER", Return)

  def arrayOf = keyword("tableau de", ArrayOf)

  def arraySize = keyword("de taille", ArraySize)

  def literalBool = "(vrai)|(faux)".r ^^ (x => LiteralBool(x equals "vrai"))

  def literalInt = raw"\d+".r ^^ (x => LiteralInt(x.toInt))

  def literalReal = raw"\d+\.\d+".r ^^ (x => LiteralReal(x.toDouble))

  def literalChar = "'[^']'".r ^^ (x => LiteralChar(x.charAt(1)))

  def literalText = "\"[^\"]*\"".r ^^ (x => LiteralText(x.substring(1, x.length-1)))

  def operator = raw"[+\-*/!]|(DIV)|(MOD)|([<>]=?)|(!?=)".r ^^ Operator.apply

  def identifier = raw"\w+".r ^^ Identifier.apply

  def tokens: Parser[List[Token]] = phrase(rep1(
    comma | dot | colon | parenthesisOpen | parenthesisClose | bracketOpen | bracketClose | assignment | program | begin
      | end | variable | function | procedure | constant | structure | enumeration | ifCond | thenCond | elseCond | and
      | or | forLoop | from | to | step | whileLoop | doToken | returnToken | arrayOf | arraySize | literalBool
      | literalReal | literalInt | literalChar | literalText | operator | identifier
  ))

  def apply(code: String): Either[ParsingError, List[Token]] = parseAll(tokens, code) match {
    case Success(result, _) => Right(result)
    case failure: Failure => Left(ParsingError(failure.toString))
    case err: Error => Left(ParsingError(err.toString))
  }
}

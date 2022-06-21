package io.github.iltotore.cylang.parse

import scala.util.parsing.combinator.RegexParsers
import Token.*

//noinspection TypeAnnotation
object ExpressionLexer extends RegexParsers {

  def comma = "," ^^ (_ => Comma)
  def dot = "." ^^ (_ => Dot)
  def colon = ":" ^^ (_ => Colon)
  def program = "PROGRAMME" ^^ (_ => Program)
  def begin = "DEBUT" ^^ (_ => Begin)
  def end = "FIN" ^^ (_ => End)
  def variable = "VARIABLE" ^^ (_ => Variable)
  def function = "FUNCTION" ^^ (_ => Function)
  def procedure = "PROCEDURE" ^^ (_ => Procedure)
  def constant = "CONSTANTE" ^^ (_ => Constant)
  def structure = "STRUCTURE" ^^ (_ => Structure)
  def enumeration = "ENUMERATION" ^^ (_ => Enumeration)
  def assignment = "<-" ^^ (_ => Assignment)
  def ifCond = "SI" ^^ (_ => If)
  def thenCond = "THEN" ^^ (_ => Then)
  def elseCond = "SINON" ^^ (_ => Else)
  def forLoop = "POUR" ^^ (_ => For)
  def from = "DE" ^^ (_ => From)
  def to = "A" ^^ (_ => To)
  def step = "PAS DE" ^^ (_ => Step)
  def whileLoop = "TANT QUE" ^^ (_ => While)
  def doToken = "FAIRE" ^^ (_ => Do)
  def returnToken = "RETOURNER" ^^ (_ => Return)
  def arrayOf = "tableau de" ^^ (_ => ArrayOf)
  def arraySize = "de taille" ^^ (_ => ArraySize)
  def literalBool = "(true)|(false)".r ^^ (x => LiteralBool(x.toBoolean))
  def literalInt = raw"\d+".r ^^ (x => LiteralInt(x.toInt))
  def literalReal = raw"\d+(.\d*)?".r ^^ (x => LiteralReal(x.toDouble))
  def literalChar = "'[^']'".r ^^ (x => LiteralChar(x.charAt(0)))
  def literalText = "\"[^\"]*\"".r ^^ LiteralText.apply
  def identifier = raw"\w+".r ^^ Identifier.apply
  def parenthesisOpen = "(" ^^ (_ => ParenthesisOpen)
  def parenthesisClose = ")" ^^ (_ => ParenthesisClose)
  def bracketOpen = "[" ^^ (_ => BracketOpen)
  def bracketClose = "]" ^^ (_ => BracketClose)
  def comparisonOperator = "([<>]=?)|(!?=)".r ^^ ComparisonOperator.apply
  def arithmeticOperator = "[+-]".r ^^ ArithmeticOperator.apply
  def termOperator = raw"[*/]|(DIV)|(MOD)".r ^^ TermOperator.apply
  def unaryOperator = raw"[+\-!]".r ^^ UnaryOperator.apply

  def tokens: Parser[List[Token]] = phrase(rep1(
    comma | dot | colon | program | begin | end | variable | function | procedure | structure | enumeration | assignment
    | ifCond | thenCond | elseCond | forLoop | from | to | step | whileLoop | doToken | returnToken | arrayOf
    | arraySize | literalBool | literalInt | literalReal | literalChar | literalText | identifier | parenthesisOpen
    | parenthesisClose | bracketOpen | bracketClose | comparisonOperator
    | arithmeticOperator | unaryOperator
  ))

  def apply(code: String): Either[ParsingError, List[Token]] = parseAll(tokens, code) match {
    case Success(result, _) => Right(result)
    case Failure(msg, _) => Left(ParsingError(msg))
    case Error(msg, _) => Left(ParsingError(msg))
  }
}

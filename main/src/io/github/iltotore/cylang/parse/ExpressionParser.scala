package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Expression, Value}

import scala.util.parsing.combinator.*

object ExpressionParser extends RegexParsers {
  
  def expression: Parser[Expression] = variableAssignment

  //Literal
  def bool: Parser[Literal] = raw"(true)|(false)".r ^^ { x => Literal(Value.Bool(x.toBoolean)) }

  def text: Parser[Literal] = "\\\".*\\\"".r ^^ { x => Literal(Value.Text(x.substring(1, x.length - 1))) }

  def character: Parser[Literal] = raw"'.'".r ^^ { x => Literal(Value.Character(x.charAt(1))) }

  def real: Parser[Literal] = raw"[0-9]+\.[0-9]+".r ^^ { x => Literal(Value.Real(x.toDouble)) }

  def integer: Parser[Literal] = raw"[0-9]+".r ^^ { x => Literal(Value.Integer(x.toLong)) }

  def literalSymbol = bool | text | character | real | integer

  //Misc
  def paranthesized = "(" ~> expression <~ ")"

  def variableCall = raw"\w+".r ^^ VariableCall.apply

  def functionCall = raw"\w+".r ~ ("(" ~> repsep(expression, ",")  <~ ")") ^^ {
    case name ~ args => FunctionCall(name, args)
  }

  private val binaryOps: Map[String, (Expression, Expression) => Expression] = Map(
    "=" -> Equality.apply,
    ">" -> Greater.apply,
    ">=" -> GreaterEqual.apply,
    "<" -> Less.apply,
    "<=" -> LessEqual.apply,
    "+" -> Addition.apply,
    "-" -> Substraction.apply,
    "*" -> Multiplication.apply,
    "/" -> Division.apply,
    "DIV" -> WholeDivision.apply,
    "%" -> Modulo.apply
  )

  //Binary Operators
  //POUR i DE 0 A 10 FAIRE
  def forLoop = "POUR" ~> raw"\w+".r ~ "DE" ~ expression ~ "A" ~ expression ~ "FAIRE" ~ tree("POUR") ^^ {
    case param ~ "DE" ~ from ~ "A" ~ to ~ "FAIRE" ~ expr => ForLoop(param, from, to, expr)
  }

  def tree(end: String) = rep(not("FIN") ~> expression) <~ (s"FIN $end") ^^ Tree.apply

  def variableAssignment = equality | (raw"\w+".r ~ "<-" ~ equality ^^ { case name ~ _ ~ expr => VariableAssignment(name, expr) })

  def equality = inequality * ("!?=".r ^^ binaryOps.apply)

  def inequality = arith * ("[<>]=?".r ^^ binaryOps.apply)

  def arith = term * ("[+-]".r ^^ binaryOps.apply)

  def term = unary * ("[*/%]|(DIV)".r ^^ binaryOps.apply)

  private val unaryOps: Map[String, Expression => Expression] = Map(
    "+" -> (x => x),
    "-" -> Negation.apply,
    "!" -> Not.apply
  )

  def unary = raw"[+\-!]".r.? ~ invocable ^^ {
    case Some(op) ~ expr => unaryOps(op)(expr)

    case None ~ expr => expr
  }

  def invocable = literalSymbol | paranthesized | functionCall | variableCall

}
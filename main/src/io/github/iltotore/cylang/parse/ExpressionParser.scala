package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Expression, Value}

import scala.util.parsing.combinator.*

object ExpressionParser extends RegexParsers {

  def expression: Parser[Expression] = variableAssignment

  //Literal
  def bool: Parser[Literal] = raw"(true)|(false)".r ^^ { x => Literal(Value.Bool(x.toBoolean)) }
  def text: Parser[Literal] = "\\\".*\\\"".r ^^ { x => Literal(Value.Text(x.substring(1, x.length-1))) }
  def character: Parser[Literal] = raw"'.'".r ^^ { x => Literal(Value.Character(x.charAt(1))) }
  def real: Parser[Literal] = raw"\-?[0-9]+\.[0-9]+".r ^^ { x => Literal(Value.Real(x.toDouble)) }
  def integer: Parser[Literal] = raw"\-?[0-9]+".r ^^ { x => Literal(Value.Integer(x.toLong)) }
  
  def literalSymbol = bool | text | character | real | integer
  def paranthesized = "(" ~> expression <~ ")"

  def variableCall = raw"\w+".r ^^ VariableCall.apply

  private val binaryOps: Map[String, (Expression, Expression) => Expression] = Map(
    "=" -> Equality.apply,
    ">" -> Greater.apply,
    ">=" -> GreaterEqual.apply,
    "<" -> Less.apply,
    "<=" -> LessEqual.apply,
    "+" -> Addition.apply,
    "-" -> Substraction.apply,
    "*" -> Multiplication.apply,
    "/" -> Division.apply
  )

  //Binary Operators
  def variableAssignment = equality | (raw"\w+".r ~ "<-" ~ equality ^^ { case name ~ "<-" ~ expr => VariableAssignment(name, expr)})
  def equality = inequality * ("!?=".r ^^ binaryOps.apply)
  def inequality = arith * ("[<>]=?".r ^^ binaryOps.apply)
  def arith = term * ("[+-]".r ^^ binaryOps.apply)
  def term = invocable * ("[*/]".r ^^ binaryOps.apply)

  //private val unaryOps: Map[String, Expression => Expression]

  //TODO Unary Operators

  def invocable = literalSymbol | paranthesized | variableCall
}
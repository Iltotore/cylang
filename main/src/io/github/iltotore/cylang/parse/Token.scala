package io.github.iltotore.cylang.parse

import scala.util.parsing.input.Positional

enum Token extends Positional {
  case Comma
  case Dot
  case Colon
  case Program
  case Begin
  case End
  case Variable
  case Function
  case Procedure
  case Constant
  case Structure
  case Enumeration
  case Assignment
  case If
  case Then
  case Else
  case For
  case From
  case To
  case Step
  case While
  case Do
  case Return
  case ArrayOf
  case ArraySize
  case LiteralBool(value: Boolean)
  case LiteralInt(value: Int)
  case LiteralReal(value: Double)
  case LiteralChar(value: Char)
  case LiteralText(value: String)
  case Identifier(value: String)
  case ParenthesisOpen
  case ParenthesisClose
  case BracketOpen
  case BracketClose
  case ComparisonOperator(op: String)
  case ArithmeticOperator(op: String)
  case TermOperator(op: String)
  case UnaryOperator(op: String)
}

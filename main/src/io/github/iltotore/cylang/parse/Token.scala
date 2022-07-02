package io.github.iltotore.cylang.parse

import scala.util.parsing.input.Positional

enum Token extends Positional {
  //Symbols
  case Comma
  case Dot
  case Colon
  case ParenthesisOpen
  case ParenthesisClose
  case BracketOpen
  case BracketClose
  case Assignment
  
  //Keywords
  case Program
  case Begin
  case End
  case Variable
  case Function
  case Procedure
  case Constant
  case Structure
  case Enumeration
  case If
  case Then
  case Else
  case And
  case Or
  case For
  case From
  case To
  case Step
  case While
  case Do
  case Return
  case ArrayOf
  case ArraySize
  
  //Misc
  case LiteralBool(value: Boolean)
  case LiteralInt(value: Int)
  case LiteralReal(value: Double)
  case LiteralChar(value: Char)
  case LiteralText(value: String)
  case Identifier(value: String)
  case Operator(value: String)
}

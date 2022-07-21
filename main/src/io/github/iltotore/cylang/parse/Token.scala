package io.github.iltotore.cylang.parse

import scala.util.parsing.input.{Position, Positional, NoPosition}

sealed trait Token extends Positional

object Token { //Note: We cannot use Dotty's enum here because we want all our cases to be case classes (to carry the position)
  //Symbols
  case class Comma() extends Token
  case class Dot() extends Token
  case class Colon() extends Token
  case class ParenthesisOpen() extends Token
  case class ParenthesisClose() extends Token
  case class BracketOpen() extends Token
  case class BracketClose() extends Token
  case class Assignment() extends Token

  //Keywords
  case class Program() extends Token
  case class Begin() extends Token
  case class End() extends Token
  case class Variable() extends Token
  case class Function() extends Token
  case class Procedure() extends Token
  case class Constant() extends Token
  case class Structure() extends Token
  case class Enumeration() extends Token
  case class If() extends Token
  case class Then() extends Token
  case class Else() extends Token
  case class And() extends Token
  case class Or() extends Token
  case class For() extends Token
  case class From() extends Token
  case class To() extends Token
  case class Step() extends Token
  case class While() extends Token
  case class Do() extends Token
  case class Return() extends Token
  case class ArrayOf() extends Token
  case class ArraySize() extends Token

  //Misc
  case class LiteralBool(value: Boolean) extends Token
  case class LiteralInt(value: Int) extends Token
  case class LiteralReal(value: Double) extends Token
  case class LiteralChar(value: Char) extends Token
  case class LiteralText(value: String) extends Token
  case class Identifier(value: String) extends Token
  case class Operator(value: String) extends Token
  case class EOF() extends Token
}

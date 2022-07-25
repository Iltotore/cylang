package io.github.iltotore.cylang.parse

import scala.util.Random
import scala.util.parsing.input.{NoPosition, Position, Positional}

sealed trait Token(name: String) extends Positional {
  
  override def toString: String = name
}

object Token { //Note: We cannot use Dotty's enum here because we want all our cases to be case classes (to carry the position)
  //Symbols
  case class Comma() extends Token(",")
  case class Dot() extends Token(".")
  case class Colon() extends Token(":")
  case class ParenthesisOpen() extends Token("(")
  case class ParenthesisClose() extends Token(")")
  case class BracketOpen() extends Token("[")
  case class BracketClose() extends Token("]")
  case class Assignment() extends Token("<-")

  //Keywords
  case class Program() extends Token("PROGRAMME")
  case class Begin() extends Token("DEBUT")
  case class End() extends Token("FIN")
  case class Variable() extends Token("VARIABLE")
  case class Function() extends Token("FONCTION")
  case class Procedure() extends Token("PROCEDURE")
  case class Constant() extends Token("CONSTANTE")
  case class Structure() extends Token("STRUCTURE")
  case class Enumeration() extends Token("ENUMERATION")
  case class If() extends Token("SI")
  case class Then() extends Token("ALORS")
  case class Else() extends Token("SINON")
  case class And() extends Token("ET")
  case class Or() extends Token("OU")
  case class For() extends Token("POUR")
  case class From() extends Token("DE")
  case class To() extends Token("A")
  case class Step() extends Token("PAS DE")
  case class While() extends Token("TANT QUE")
  case class Do() extends Token("FAIRE")
  case class Return() extends Token("RETOURNER")
  case class ArrayOf() extends Token("tableau de")
  case class ArraySize() extends Token("de taille")

  //Misc
  case class LiteralBool(value: Boolean) extends Token(value.toString)
  case class LiteralInt(value: Int) extends Token(value.toString)
  case class LiteralReal(value: Double) extends Token(value.toString)
  case class LiteralChar(value: Char) extends Token(value.toString)
  case class LiteralText(value: String) extends Token(value)
  case class Identifier(value: String) extends Token(value)
  case class Operator(value: String) extends Token(value)
  case class EOF() extends Token("FIN DE FICHIER")
}

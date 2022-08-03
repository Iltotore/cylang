package io.github.iltotore.cylang.parse

import scala.util.parsing.input.{NoPosition, Position, Reader}

/**
 * A Reader that consume [[Token]]s
 * @param tokens the tokens to consume
 */
class TokenReader(tokens: List[Token]) extends Reader[Token]{

  override def first: Token = if(atEnd) Token.EOF() else tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = first.pos
  override def rest: Reader[Token] = new TokenReader(tokens.tail)

  override def toString: String = s"Remaining tokens: ${tokens.map(t => s"$t: ${t.pos}")}"
}

package io.github.iltotore.cylang.parse

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Position

/**
 * Parsing utils
 */
trait CYParsers extends Parsers {

  def successWithPos[T](value: Position ?=> T): Parser[T] = new Parser {

    override def apply(in: Input): ParseResult[T] = Success(value(using in.pos), in)
  }

  extension [T](p: Parser[T]) {

    def mapWithPos[U](f: Position ?=> T => U): Parser[U] = new Parser {

      override def apply(in: Input): ParseResult[U] = {
        p(in).map(f(using in.pos))
      }
    }

    def partialMapWithPos[U](f: Position ?=> PartialFunction[T, U]): Parser[U] = new Parser {

      override def apply(in: Input): ParseResult[U] = {
        p(in).mapPartial(f(using in.pos), r => s"Constructor function not defined at $r")
      }
    }
  }
  
}

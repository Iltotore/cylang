package io.github.iltotore.cylang.parse

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.*
import scala.util.parsing.input.Position

trait CYParsers extends Parsers {

  case class &[+A, +B](_1: A, _2: B) {
    override def toString: String = s"$_1&$_2"
  }

  def successWithPos[T](value: Position ?=> T): Parser[T] = new Parser {

    override def apply(in: Input): ParseResult[T] = Success(value(using in.pos), in)
  }

  extension [T](p: Parser[T]) {

    /**
     * A parser combinator for parallel composition.
     * `p & q` succeeds if both `p` and `q` do on the same input.
     * @param q a parser that will be executed with the same input as `p`
     * @return A `Parser` that returns -- on success -- `p & q` with the higher input leftover.
     */
    infix def &[U](q: => Parser[U]): Parser[&[T, U]] = new Parser {

      override def apply(in: Input): ParseResult[&[T, U]] = (p(in), q(in)) match {
        case (Success(result1, next1), Success(result2, next2)) => Success(new &(result1, result2), if(next2.pos < next1.pos || next2.pos == next1.pos) next1 else next2)
        case (s1: Success[?], ns2: NoSuccess) => ns2
        case (ns1: NoSuccess, s2: Success[?]) => ns1
        case (ns1: NoSuccess, ns2: NoSuccess) => ns1.append(ns2)
      }
    }

    infix def \[U](q: => Parser[U]): Parser[T] = skip(q)

    def skip[U](q: => Parser[U]): Parser[T] = q.* ~> p

    def mapWithPos[U](f: Position ?=> T => U): Parser[U] = new Parser {

      override def apply(in: Input): ParseResult[U] = res.map(f(using in))
    }
  }
}

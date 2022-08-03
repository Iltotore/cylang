package io.github.iltotore.cylang.parse

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

/**
 * A Parser with french error messages
 */
trait FrenchParser extends Parsers {

  extension[A] (parser: Parser[A]) {

    def mapResult[B](f: ParseResult[A] => ParseResult[B]): Parser[B] = new Parser {

      override def apply(in: Input): ParseResult[B] = f(parser(in))
    }
  }

  implicit override def accept(e: Elem): Parser[Elem] = acceptIf(_ == e)(got => s"'$got' trouvé à la place de '$e'")

  override def phrase[T](p: Parser[T]): Parser[T] = new Parser[T] {
    def apply(in: Input) = p(in) match {
      case s@Success(_, in1) =>
        if (in1.atEnd) s
        else s.lastFailure match {
          case Some(failure) => failure
          case _ => Failure("Fin de fichier attendu", in1)
        }
      case ns => ns
    }
  }
  override def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] = super.acceptIf(p)(err) mapResult {

    case Failure(_, next) if next.atEnd => Failure("Fin de fichier", next)

    case other => other
  }


}

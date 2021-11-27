package io.github.iltotore.cylang

import scala.collection.mutable

package object util {

  extension[A, B] (either: Either[A, B]) {

    def zip[C](other: Either[A, C]): Either[A, (B, C)] = (either, other) match {

      case (Right(x), Right(y)) => Right((x, y))

      case (Left(x), _) => Left(x)

      case (_, Left(y)) => Left(y)
    }
  }

  extension[A, B] (list: List[Either[A, B]]) {

    def failFast: Either[A, List[B]] = Right(
      for (either <- list) yield either match {

        case Right(value) => value

        case Left(value) => return Left(value)
      }
    )
  }
}
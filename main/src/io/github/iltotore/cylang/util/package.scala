package io.github.iltotore.cylang

import scala.collection.mutable
import scala.reflect.ClassTag

package object util extends EitherCapability {

  extension[A, B] (either: Either[A, B]) {

    def zip[C](other: Either[A, C]): Either[A, (B, C)] = (either, other) match {

      case (Right(x), Right(y)) => Right((x, y))

      case (Left(x), _) => Left(x)

      case (_, Left(y)) => Left(y)
    }
  }
}
package io.github.iltotore.cylang

import scala.quoted.*
import scala.collection.mutable
import scala.reflect.ClassTag

package object util extends EitherCapability {

  extension[A <: Throwable, B](either: Either[A, B]) {

    /**
     * Get the right value or throw the left error.
     * @return the right value of throw an error of type `A`
     */
    def orThrowLeft: B = either match {

      case Right(value) => value
      case Left(throwable) => throw throwable
    }
  }
}
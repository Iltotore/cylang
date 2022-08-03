package io.github.iltotore.cylang

import scala.collection.mutable
import scala.quoted.*
import scala.reflect.ClassTag

package object util extends EitherCapability {

  extension[A <: Throwable, B] (either: Either[A, B]) {

    /**
     * Get the right value or throw the left error.
     *
     * @return the right value of throw an error of type `A`
     */
    def orThrowLeft: B = either match {

      case Right(value) => value
      case Left(throwable) => throw throwable
    }
  }

  extension (str: String) {

    /**
     * Replacement to [[String.stripTrailing]] which is not (yet) supported by ScalaNative.
     *
     * @return this [[String]] without trailing space or line break.
     * @see [[String.stripTrailing]]
     */
    def stripTrailingWhitespaces: String = {
      val builder = mutable.StringBuilder()
      var buffer = mutable.StringBuilder()
      for (chr <- str) {
        if (chr == ' ' || chr == '\n') buffer += chr
        else {
          if (buffer.nonEmpty) {
            builder ++= buffer
            buffer = mutable.StringBuilder()
          }

          builder += chr
        }
      }
      builder.toString()
    }
  }
}
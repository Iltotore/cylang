package io.github.iltotore.cylang.util

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait EitherCapability {

  /**
   * Represent the imperative-styled Either DSL.
   * @tparam L
   */
  final class EitherDSL[L]

  /**
   * An exception-based short-circuit to return a left value.
   * @param left the returned left value
   * @tparam L the left type
   */
  case class ReturnLeft[L](left: L) extends Throwable

  /**
   * Entrypoint for the imperative-style either DSL.
   * @param body the function which can use the DSL
   * @param tag the [[ClassTag]] of the `L` type
   * @tparam L the left/error type
   * @tparam R the right/valid type
   * @return the result (right) or the thrown error (left)
   */
  def either[L, R](body: EitherDSL[L] ?=> R)(using tag: ClassTag[L]): Either[L, R] = {
    Try(body(using new EitherDSL[L])) match {

      case Success(right) => Right(right)

      case Failure(ReturnLeft(left)) if tag.runtimeClass.isInstance(left) => Left(left.asInstanceOf[L])

      case Failure(exception) => throw exception
    }
  }

  /**
   * Throw a [[ReturnLeft]] error with a left value.
   * @param left the left value to return
   * @param EitherDSL[L] to restrict the usage of this method
   * @tparam L the left type
   */
  def left[L](left: L)(using EitherDSL[L]): Nothing = throw ReturnLeft(left)

  /**
   * Get the right value of throw the left.
   * @param value the either to get from
   * @param EitherDSL[L] to restrict the usage of this method
   * @tparam L the left type
   * @tparam R the right type
   * @return the right value if exist. Throw exception instead
   */
  def ensureRight[L, R](value: Either[L, R])(using EitherDSL[L]): R = value match {
    case Right(rightValue) => rightValue
    case Left(leftValue) => left(leftValue)
  }

  /**
   * Get the optional value if exist or throw the given left.
   * @param value the [[Option]] to get from
   * @param leftValue the value to throw if `value` is [[None]]
   * @param EitherDSL[L] to restrict the usage of this method
   * @tparam L the left type
   * @tparam R the right type
   * @return the right value if exist. Throw exception instead
   */
  def ensureOption[L, R](value: Option[R])(leftValue: => L)(using EitherDSL[L]): R = ensureRight(value.toRight(leftValue))
}

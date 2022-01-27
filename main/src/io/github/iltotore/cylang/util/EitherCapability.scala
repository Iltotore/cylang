package io.github.iltotore.cylang.util

import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

trait EitherCapability {

  final class EitherDSL[L]

  case class ReturnLeft[L](left: L) extends Throwable

  def either[L, R](body: EitherDSL[L] ?=> R)(using tag: ClassTag[L]): Either[L, R] = {
    Try(body(using new EitherDSL[L])) match {

      case Success(right) => Right(right)

      case Failure(ReturnLeft(left)) if tag.runtimeClass.isInstance(left) => Left(left.asInstanceOf[L])

      case Failure(exception) => throw exception
    }
  }

  def left[L](left: L)(using EitherDSL[L]): Nothing = throw ReturnLeft(left)

  def ensureRight[L, R](value: Either[L, R])(using EitherDSL[L]): R = value match {
    case Right(rightValue) => rightValue
    case Left(leftValue) => left(leftValue)
  }

  def ensureOption[L, R](value: Option[R])(leftValue: => L)(using EitherDSL[L]): R = ensureRight(value.toRight(leftValue))
}

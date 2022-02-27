package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.{CYType, Context, Cursor}

case class EvaluationError(message: String, stack: List[Cursor]) extends Exception(message) {

  lazy val fullMessage: String = {

    val s = stack.reverse

    if (stack.isEmpty) s"Erreur: $message"
    else
      s"""Erreur: $message
         |
         |${s.head.trace}
         |
         |${s.head.position.longString}
         |
         |
         |${s.tail.map(_.trace).mkString("\n")}""".stripMargin
  }

  override def toString: String = fullMessage
}

object EvaluationError {

  def apply(message: String)(using context: Context): EvaluationError = EvaluationError(message, context.stack)

  def impossible(stack: List[Cursor]): EvaluationError = EvaluationError("This is a language bug!", stack)

  def impossible(using context: Context): EvaluationError = impossible(context.stack)

  def typeMismatch(got: Any)(using Context): EvaluationError = EvaluationError(s"Type incompatible pour la valeur $got")
}

package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.{CYError, CYType, Context, Cursor}

case class EvaluationError(message: String, stack: List[Cursor]) extends Error(message) with CYError {

  lazy val fullMessage: String = {

    val distinctStack = stack.distinct

    if (stack.isEmpty) s"Erreur: $message"
    else
      s"""Erreur: $message
         |
         |${distinctStack.head.trace}
         |
         |${distinctStack.head.position.longString}
         |
         |
         |${distinctStack.tail.map(_.trace).mkString("\n")}""".stripMargin
  }

  override def toString: String = fullMessage
}

object EvaluationError {

  def apply(message: String)(using context: Context): EvaluationError = EvaluationError(message, context.stack)

  def impossible(stack: List[Cursor]): EvaluationError = EvaluationError("Situation impossible. Probablement un bug", stack)

  def impossible(using context: Context): EvaluationError = impossible(context.stack)

  def typeMismatch(got: Any)(using Context): EvaluationError = EvaluationError(s"Type incompatible pour la valeur $got")
}

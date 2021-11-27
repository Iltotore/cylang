package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.Context

case class EvaluationError(message: String, stack: List[Int]) extends Exception(message)

object EvaluationError {
  
  def apply(message: String)(using context: Context): EvaluationError = EvaluationError(message, context.stack)
  
  def impossible(stack: List[Int]): EvaluationError = EvaluationError("This is a language bug!", stack)
  
  def impossible(using context: Context): EvaluationError = impossible(context.stack)
}

package io.github.iltotore

import io.github.iltotore.cylang.ast.*
import io.github.iltotore.cylang.eval.{*, given}
import io.github.iltotore.cylang.parse.ExpressionParser.*
import io.github.iltotore.cylang.parse.ParsingException

package object cylang {

  def execute(source: String)(using context: Context, evaluator: Evaluator[Expression]): Either[ParsingException | EvaluationError, (Context, Value)] =
    parseAll(program, source) match {

      case Success(result, _) => result.evaluate

      case Failure(msg, _) => Left(ParsingException(msg))

      case Error(msg, _) => Left(ParsingException(msg))
    }

}

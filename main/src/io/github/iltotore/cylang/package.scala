package io.github.iltotore

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.eval.*
import io.github.iltotore.cylang.parse.ExpressionParser.*
import io.github.iltotore.cylang.eval.given_Evaluator_Expression.evaluate
import io.github.iltotore.cylang.parse.ParsingException

package object cylang {

  def execute(source: String)(using context: Context = Context.empty): Either[ParsingException | EvaluationError, (Context, Value)] =
    parseAll(program, source) match {

      case Success(result, _) => result.evaluate

      case Failure(msg, _) => Left(ParsingException(msg))
    }

}

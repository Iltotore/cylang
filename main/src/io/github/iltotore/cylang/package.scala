package io.github.iltotore

import io.github.iltotore.cylang.ast.*
import io.github.iltotore.cylang.eval.{*, given}
import io.github.iltotore.cylang.parse.ExpressionParser.*
import io.github.iltotore.cylang.parse.{ExpressionLexer, ExpressionParser, ParsingError}

package object cylang {
  
  def execute(source: String)(using context: Context, evaluator: Evaluator[Expression]): Either[ParsingError | EvaluationError, (Context, Value)] =
    for {
      tokens <- ExpressionLexer(source)
      expression <- ExpressionParser(tokens)
      result <- expression.evaluate
    } yield result

}

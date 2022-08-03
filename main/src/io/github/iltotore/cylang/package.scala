package io.github.iltotore

import io.github.iltotore.cylang.ast.*
import io.github.iltotore.cylang.eval.{*, given}
import io.github.iltotore.cylang.parse.ExpressionParser.*
import io.github.iltotore.cylang.parse.{ExpressionLexer, ExpressionParser, ParsingError}

package object cylang {

  /**
   * Execute the given source.
   * @param source the source code to parse and evaluate
   * @param context the context to use as base
   * @param evaluator the evaluator to evaluate the parsed AST
   * @return the final result of this execution
   */
  def execute(source: String)(using context: Context, evaluator: Evaluator[Expression]): Either[ParsingError | EvaluationError, (Context, Value)] =
    for {
      tokens <- ExpressionLexer(source)
      expression <- ExpressionParser(tokens)
      result <- expression.evaluate
    } yield result

}

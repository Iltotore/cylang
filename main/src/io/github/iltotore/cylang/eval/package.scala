package io.github.iltotore.cylang

import scala.util.{Failure, Success, Try}
import io.github.iltotore.cylang.ast.{Expression, Value}
import io.github.iltotore.cylang.CYType

package object eval {

  type EvalResult = Either[EvaluationError, (Context, Value)]

  type Evaluation = Context ?=> EvalResult

  given Evaluator[Expression] = new ExpressionEvaluator
  
}
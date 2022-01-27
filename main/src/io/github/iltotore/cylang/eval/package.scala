package io.github.iltotore.cylang

import scala.util.{Failure, Success, Try}
import io.github.iltotore.cylang.ast.{Expression, Value}

package object eval {

  type EvalResult = Either[EvaluationError, (Context, Value)]

  type Evaluation = Context ?=> EvalResult

  given Evaluator[Expression] = new ExpressionEvaluator

  extension [A](input: A)(using evaluator: Evaluator[A]) {

    def evaluate(using Context) = evaluator.evaluate(input)
  }

  case class EvalDSL(var context: Context)

  def eval(ev: EvalDSL ?=> Value)(using context: Context): EvalResult = {
    val dsl = EvalDSL(context)
    Try(ev(using dsl)) match {

      case Success(value) => Right((dsl.context, value))

      case Failure(err@EvaluationError(_, _)) => Left(err)

      case Failure(exception) => throw exception
    }
  }

  given currentContext(using dsl: EvalDSL): Context = dsl.context

  def abort[T](message: String)(using EvalDSL): T = throw EvaluationError(message)

  inline def ??[T](using EvalDSL): T = abort("Impossible")

  def unbox(result: EvalResult)(using dsl: EvalDSL): Value = result match {

    case Right((ctx, value)) =>
      update(ctx)
      value

    case Left(value) => throw value
  }

  def evalUnbox[A: Evaluator](toEvaluate: A)(using dsl: EvalDSL): Value = unbox(toEvaluate.evaluate)

  def partialUnbox(result: EvalResult)(using dsl: EvalDSL): (Context, Value) = result match {

    case Right(x) => x

    case Left(value) => throw value
  }

  def update(context: Context)(using dsl: EvalDSL): Unit = dsl.context = dsl.context.merged(context)
}
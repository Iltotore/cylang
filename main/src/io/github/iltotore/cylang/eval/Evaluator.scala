package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.ast.Value
import io.github.iltotore.cylang.{Context, Cursor}

import scala.util.{Failure, Success, Try}

/**
 * Capability to be evaluated.
 *
 * @tparam A the evaluable type
 */
trait Evaluator[-A] {

  /**
   * Evaluate the given input.
   *
   * @param input   the input to evaluate
   * @param Context the context to use for evaluation
   * @return
   */
  def evaluateInput(input: A)(using Context): EvalResult

  extension (input: A) {

    def evaluate(using ctx: Context): EvalResult = evaluateInput(input)
  }

  //Evaluation DSL

  /**
   * Evaluation DSL entrypoint.
   *
   * @param ev      the function which can use the DSL
   * @param context the context to embed
   * @return the result of this evaluation
   */
  def eval(ev: EvalDSL ?=> Value)(using context: Context): EvalResult = {
    val dsl = EvalDSL(context)
    Try(ev(using dsl)) match {

      case Success(value) => Right((dsl.context, value))

      case Failure(err@EvaluationError(_, _)) => Left(err)

      case Failure(exception) => throw exception
    }
  }

  /**
   * The current embedded context of this DSL.
   *
   * @param dsl to restrict the usage of this method
   * @return the embedded context
   */
  given currentContext(using dsl: EvalDSL): Context = dsl.context

  /**
   * Throw an [[EvaluationError]] with a message.
   *
   * @param message the abortion message
   * @param EvalDSL to restrict the usage of this method
   */
  inline def abort(message: String)(using EvalDSL): Nothing = throw EvaluationError(message)

  /**
   * Evaluate the given input then unbox the result.
   *
   * @param toEvaluate the input to evaluate
   * @param EvalDSL    to restrict the usage of this method
   * @return the [[Value]] of the produced [[EvalResult]]. Use the result's [[Context]] as new current context
   */
  def evalUnbox(toEvaluate: A)(using EvalDSL): Value = unbox(toEvaluate.evaluate)

  /**
   * Extract the given [[EvalResult]].
   *
   * @param result the result to unbox
   * @param dsl    to restrict the usage of this method
   * @return the [[Value]] of the given [[EvalResult]]. Use the result's [[Context]] as new current context
   */
  def unbox(result: EvalResult)(using dsl: EvalDSL): Value = result match {

    case Right((ctx, value)) =>
      update(ctx)
      value

    case Left(value) => throw value
  }

  /**
   * Use the given context as new current.
   *
   * @param context the context to use
   * @param dsl     to restrict the usage of this method
   */
  def update(context: Context)(using dsl: EvalDSL): Unit =
    dsl.context = context.copy(currentFunction = dsl.context.currentFunction, stack = dsl.context.stack)

  /**
   * Extract the given [[EvalResult]].
   *
   * @param result the result to unbox
   * @param dsl    to restrict the usage of this method
   * @return the [[Value]] and [[Context]] of the given [[EvalResult]]. Use the result's [[Context]] as new current context
   */
  def partialUnbox(result: EvalResult)(using EvalDSL): (Context, Value) = result match {

    case Right(x) => x

    case Left(value) => throw value
  }

  /**
   * Represent the imperative-styled evaluation DSL.
   *
   * @param context the embedded context
   */
  case class EvalDSL(var context: Context)
}
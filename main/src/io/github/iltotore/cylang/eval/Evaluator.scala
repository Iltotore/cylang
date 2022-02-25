package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.{Context, Cursor}
import io.github.iltotore.cylang.ast.Value

import scala.util.{Failure, Success, Try}

trait Evaluator[-A] {
  
    def evaluateInput(input: A)(using Context): EvalResult

    extension (input: A) {

        def evaluate(using Context): EvalResult = evaluateInput(input)
    }

    //Evaluation DSL

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
    
    def abort(message: String)(using EvalDSL): Nothing = throw EvaluationError(message)

    inline def ??(using EvalDSL): Nothing = abort("Impossible")

    def unbox(result: EvalResult)(using dsl: EvalDSL): Value = result match {

        case Right((ctx, value)) =>
            update(ctx)
            value

        case Left(value) => throw value
    }

    def evalUnbox(toEvaluate: A)(using dsl: EvalDSL): Value = unbox(toEvaluate.evaluate)

    def partialUnbox(result: EvalResult)(using dsl: EvalDSL): (Context, Value) = result match {

        case Right(x) => x

        case Left(value) => throw value
    }

    def update(context: Context)(using dsl: EvalDSL): Unit = dsl.context = context
}
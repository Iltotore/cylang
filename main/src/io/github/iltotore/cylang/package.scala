package io.github.iltotore

import io.github.iltotore.cylang.ast.{CYFunction, Value}
import io.github.iltotore.cylang.eval.EvaluationError

import scala.util.Random

package object cylang {

  type Result[T] = Either[EvaluationError, T]

  val builtin: Map[String, CYFunction] = Map(
    "stdEcrire" -> CYFunction.Builtin(CYType.Void, List(Parameter("msg", CYType.Text)), Map.empty, context => context.scope.variables("msg").value match {
      case Value(msg) =>
        println(msg)
        Right((context, Value.Void))
      case _ => Left(EvaluationError("impossible")(using context))

    }),
    "stdSqrt" -> CYFunction.Builtin(CYType.Real, List(Parameter("x", CYType.Real)), Map.empty, context => context.scope.variables("x").value match {
      case Value.Number(x) => Right((context, Value.Real(Math.sqrt(x))))
      case _ => Left(EvaluationError("impossible")(using context))
    }),
    "stdAlea" -> CYFunction.Builtin(CYType.Integer, List(Parameter("x", CYType.Integer)), Map.empty, context => context.scope.variables("x").value match {
      case Value.Integer(x) => Right((context, Value.Integer(Random.nextLong(x))))
      case _ => Left(EvaluationError("impossible")(using context))

    })
  )
}

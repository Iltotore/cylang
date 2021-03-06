package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.eval.{*, given}
import io.github.iltotore.cylang.{CYType, Context, Parameter, Scope}

trait CYFunction {

  def tpe: CYType

  def parameters: List[Parameter]

  def variables: Map[String, (CYType, Value)]

  def evaluate(args: List[Value])(using context: Context, evaluator: Evaluator[Expression]): EvalResult = {
    var scope = context.scope
    for ((param, arg) <- parameters.zip(args)) scope = scope.withDeclaration(param.name, param.tpe, arg)
    for ((name, (tpe, value)) <- variables) scope = scope.withDeclaration(name, tpe, value)
    execute(using context.copy(scope = scope))
  }

  def execute(using Context, Evaluator[Expression]): EvalResult

}

object CYFunction {

  case class UserDefined(
                          tpe: CYType,
                          parameters: List[Parameter],
                          variables: Map[String, (CYType, Value)],
                          expression: Expression
                        ) extends CYFunction {

    override def execute(using Context, Evaluator[Expression]): EvalResult = expression.evaluate
  }

  case class Builtin(
                      tpe: CYType,
                      parameters: List[Parameter],
                      variables: Map[String, (CYType, Value)],
                      function: Context => EvalResult
                    ) extends CYFunction {

    override def execute(using context: Context, evaluator: Evaluator[Expression]): EvalResult = function(context)
  }

  def apply(
             tpe: CYType,
             parameters: List[Parameter],
             variables: Map[String, (CYType, Value)],
             expression: Expression
           ): CYFunction = UserDefined(tpe, parameters, variables, expression)
}
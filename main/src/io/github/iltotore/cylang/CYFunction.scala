package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.{Expression, Value}
import io.github.iltotore.cylang.eval.{*, given}

case class CYFunction(tpe: CYType, parameters: List[Parameter], variables: Map[String, (CYType, Value)], expression: Expression) {

  def evaluate(args: List[Value])(using context: Context): EvalResult = {
    var scope: Scope = context.scope
    for((param, arg) <- parameters.zip(args)) scope = scope.withDeclaration(param.name, param.tpe, arg)
    for((name, (tpe, value)) <- variables) scope = scope.withDeclaration(name, tpe, value)
    expression.evaluate(using context.copy(scope = scope))
  }

}
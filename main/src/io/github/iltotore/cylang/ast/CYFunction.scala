package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.eval.{*, given}
import io.github.iltotore.cylang.{CYType, Context, Parameter, Scope}

/**
 * Represent a callable function
 */
trait CYFunction {

  /**
   * The return type of this function
   */
  def tpe: CYType

  /**
   * The parameters of this function
   */
  def parameters: List[Parameter]

  /**
   * The local variable declarations of this function
   */
  def variables: Map[String, (CYType, Value)]

  /**
   * Add the local variables and given arguments to the scope then execute this function.
   *
   * @param args      the arguments passed to this function
   * @param context   the current context used to evaluate this function
   * @param evaluator the expression evaluator, used to execute this function
   * @return the result of this evaluation, successful or not
   */
  def evaluate(args: List[Value])(using context: Context, evaluator: Evaluator[Expression]): EvalResult = {
    var scope = context.scope
    for ((param, arg) <- parameters.zip(args)) scope = scope.withDeclaration(param.name, param.tpe, arg)
    for ((name, (tpe, value)) <- variables) scope = scope.withDeclaration(name, tpe, value)
    execute(using context.copy(scope = scope))
  }

  /**
   * Run this function.
   *
   * @param Context   the current context used to evaluate this function
   * @param Evaluator [Expression] the expression evaluator, used to execute this function
   * @return the execution result, successful or not
   */
  def execute(using Context, Evaluator[Expression]): EvalResult

}

object CYFunction {

  export UserDefined.apply

  /**
   * A function defined by the user from the code.
   *
   * @param tpe        the return type of this function
   * @param parameters the parameters of this function
   * @param variables  the local variable declarations of this function
   * @param expression the expression to be evaluated when calling this function
   */
  case class UserDefined(
                          tpe: CYType,
                          parameters: List[Parameter],
                          variables: Map[String, (CYType, Value)],
                          expression: Expression
                        ) extends CYFunction {

    override def execute(using Context, Evaluator[Expression]): EvalResult = expression.evaluate
  }

  /**
   * Represent a builtin function which cannot be expressed using CYLang.
   *
   * @param tpe        the return type of this function
   * @param parameters the parameters of this function
   * @param variables  the local variable declarations of this function
   * @param function   the function to call when this function is executed
   */
  case class Builtin(
                      tpe: CYType,
                      parameters: List[Parameter],
                      variables: Map[String, (CYType, Value)],
                      function: Context => EvalResult
                    ) extends CYFunction {

    override def execute(using context: Context, evaluator: Evaluator[Expression]): EvalResult = function(context)
  }
}
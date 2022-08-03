package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.*
import io.github.iltotore.cylang.eval.{EvalResult, EvaluationError, Evaluator}

import scala.util.Random

/**
 * The current scope of an evaluation.
 *
 * @param enumerations the declared enumerations
 * @param structures   the declared structures
 * @param functions    the declared functions
 * @param variables    the constants and locally-declared variables
 */
case class Scope(
                  enumerations: Map[String, Enumeration],
                  structures: Map[String, Structure],
                  functions: Map[String, CYFunction],
                  variables: Map[String, Variable]
                ) {

  /**
   * Add a new function to the scope.
   *
   * @param name     the function name
   * @param function the function instance
   * @return a copy of this scope containing the given function
   */
  def withFunction(name: String, function: CYFunction): Scope = this.copy(functions = functions.updated(name, function))

  /**
   * Add a new enumeration to the scope.
   *
   * @param name        the enumeration name
   * @param enumeration the enumeration instance
   * @return a copy of this scope containing the given enumeration
   */
  def withEnumeration(name: String, enumeration: Enumeration): Scope = this.copy(enumerations = enumerations.updated(name, enumeration))

  /**
   * Add a new function to the scope.
   *
   * @param name      the function name
   * @param structure the structure instance
   * @return a copy of this scope containing the given structure
   */
  def withStructure(name: String, structure: Structure): Scope = this.copy(structures = structures.updated(name, structure))

  /**
   * Add a new variable to the scope.
   *
   * @param name    the variable name
   * @param tpe     the variable type
   * @param value   the variable's current value
   * @param mutable whether the variable is mutable or not
   * @return a copy of this scope containing the new variable
   */
  def withDeclaration(name: String, tpe: CYType, value: Value, mutable: Boolean = true): Scope = this.withVariable(name, Variable(tpe, value, mutable))

  /**
   * Add a new variable to the scope.
   *
   * @param name     the variable name
   * @param variable the variable instance
   * @return a copy of this scope containing the given variable
   */
  def withVariable(name: String, variable: Variable): Scope = this.copy(variables = variables.updated(name, variable))

  /**
   * Assign a new value to the specified variable.
   *
   * @param name    the name of the variable to mutate.
   * @param value   the value to assign to the variable
   * @param Context the current evaluation context
   * @return a copy of this scope containing the new variable, or an [[EvaluationError]]
   */
  def withAssignment(name: String, value: Value)(using Context): Either[EvaluationError, Scope] = variables.get(name) match {

    case Some(variable) => Either.cond(variable.mutable, this.withVariable(name, variable.copy(value = value)), EvaluationError(s"Impossible de modifier une constante"))

    case None => Left(EvaluationError(s"La variable '$name' n'existe pas"))
  }
}

object Scope {

  /**
   * The default scope. Only contains builtin functions which are not representable in CYLang.
   */
  val default: Scope = Scope(
    enumerations = Map.empty,
    structures = Map.empty,
    functions = Map(
      "stdEcrire" -> CYFunction.Builtin(
        tpe = CYType.Void,
        parameters = List(Parameter("x", CYType.Any)),
        variables = Map.empty,
        function = ctx => {
          ctx.out.println(ctx.scope.variables("x").value.valueToString)
          Right((ctx, Value.Void))
        }
      ),
      "stdSqrt" -> CYFunction.Builtin(
        tpe = CYType.Real,
        parameters = List(Parameter("x", CYType.Real)),
        variables = Map.empty,
        function = (ctx: Context) => ctx.scope.variables("x").value match {

          case Value.Number(x) if x >= 0 => Right((ctx, Value.Real(Math.sqrt(x))))

          case Value.Number(x) => Left(EvaluationError(s"x doit être positif. Valeur actuelle: $x")(using ctx))

          case value => Left(EvaluationError.typeMismatch(value)(using ctx))
        }
      ),
      "stdAlea" -> CYFunction.Builtin(
        tpe = CYType.Integer,
        parameters = List(Parameter("x", CYType.Integer)),
        variables = Map.empty,
        function = (ctx: Context) => ctx.scope.variables("x").value match {

          case Value.Integer(x) if x >= 0 => Right((ctx, Value.Integer(Random.nextInt(x))))

          case Value.Integer(x) => Left(EvaluationError(s"x doit être positif. Valeur actuelle: $x")(using ctx))

          case value => Left(EvaluationError.typeMismatch(value)(using ctx))
        }
      )
    ),
    variables = Map.empty
  )
}

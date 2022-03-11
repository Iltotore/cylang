package io.github.iltotore.cylang

import scala.util.Random
import io.github.iltotore.cylang.ast.{CYFunction, Enumeration, Expression, Structure, Value}
import io.github.iltotore.cylang.eval.{EvalResult, EvaluationError, Evaluator}

case class Scope(
                  enumerations: Map[String, Enumeration],
                  structures: Map[String, Structure],
                  functions: Map[String, CYFunction],
                  variables: Map[String, Variable]
                ) {

  def withVariable(name: String, variable: Variable): Scope = this.copy(variables = variables.updated(name, variable))

  def withFunction(name: String, function: CYFunction): Scope = this.copy(functions = functions.updated(name, function))

  def withEnumeration(name: String, enumeration: Enumeration): Scope = this.copy(enumerations = enumerations.updated(name, enumeration))
  
  def withStructure(name: String, structure: Structure): Scope = this.copy(structures = structures.updated(name, structure))
  
  def withDeclaration(name: String, tpe: CYType, value: Value, mutable: Boolean = true): Scope = this.withVariable(name, Variable(tpe, value, mutable))
  
  def withAssignment(name: String, value: Value)(using Context): Either[EvaluationError, Scope] = variables.get(name) match {

    case Some(variable) => Either.cond(variable.mutable, this.withVariable(name, variable.copy(value = value)), EvaluationError(s"This variable is immutable"))

    case None => Left(EvaluationError(s"Unknown variable $name"))
  }
}

object Scope {
  
  val empty: Scope = Scope(
    enumerations = Map.empty,
    structures = Map.empty,
    functions = Map(
      "stdEcrire" -> CYFunction.Builtin(
        tpe = CYType.Void,
        parameters = List(Parameter("x", CYType.Any)),
        variables = Map.empty,
        function = ctx => {
          ctx.out.println(ctx.scope.variables("x").value.value)
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

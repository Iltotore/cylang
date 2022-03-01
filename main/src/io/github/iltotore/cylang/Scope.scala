package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.{CYFunction, Enumeration, Structure, Value}
import io.github.iltotore.cylang.eval.EvaluationError

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
  
  val empty: Scope = Scope(Map.empty, Map.empty, Map.empty, Map.empty)
}

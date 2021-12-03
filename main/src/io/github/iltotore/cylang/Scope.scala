package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.{CYFunction, Value}

case class Scope(depth: Int, functions: Map[String, CYFunction], variables: Map[String, Variable]) {

  def withVariable(name: String, variable: Variable): Scope = this.copy(variables = variables.updated(name, variable))

  def withFunction(name: String, function: CYFunction): Scope = this.copy(functions = functions.updated(name, function))

  def withDeclaration(name: String, tpe: CYType, value: Value): Scope = this.withVariable(name, Variable(tpe, value, depth))
  
  def withAssignment(name: String, value: Value): Scope = variables.get(name) match {

    case Some(variable) => this.withVariable(name, variable.copy(value = value))

    case None => this
  }

  def nested: Scope = this.copy(depth = this.depth + 1)

  def merged(scope: Scope): Scope = this.copy(
    variables = this.variables ++ scope.variables.filter(_._2.depth <= this.depth),
    functions = this.functions ++ scope.functions
  )
}

object Scope {
  
  val empty: Scope = Scope(0, Map.empty, Map.empty)
}

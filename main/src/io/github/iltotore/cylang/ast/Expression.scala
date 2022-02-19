package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.eval.EvaluationError
import io.github.iltotore.cylang.{CYType, Context, Parameter, Scope, Variable}


enum Expression {

  case Empty

  case Literal(value: Value)

  case Negation(expression: Expression)

  case Addition(left: Expression, right: Expression)

  case Subtraction(left: Expression, right: Expression)

  case Multiplication(left: Expression, right: Expression)

  case Division(left: Expression, right: Expression)

  case WholeDivision(left: Expression, right: Expression)

  case Modulo(left: Expression, right: Expression)

  case Equality(left: Expression, right: Expression)

  case Greater(left: Expression, right: Expression)

  case GreaterEqual(left: Expression, right: Expression)

  case Less(left: Expression, right: Expression)

  case LessEqual(left: Expression, right: Expression)

  case Not(expression: Expression)

  case And(left: Expression, right: Expression)

  case Or(left: Expression, right: Expression)

  case VariableCall(name: String)

  case VariableAssignment(name: String, expression: Expression)

  case ArrayCall(arrayExpr: Expression, index: Expression)

  case ArrayAssignment(arrayExpr: Expression, index: Expression, expression: Expression)

  case StructureCall(structureExpr: Expression, name: String)

  case StructureAssignment(structureExpr: Expression, name: String, expression: Expression)

  case FunctionCall(name: String, args: List[Expression])

  case ForLoop(name: String, from: Expression, to: Expression, step: Expression, expression: Expression)

  case WhileLoop(condition: Expression, expression: Expression)

  case DoWhileLoop(condition: Expression, expression: Expression)

  case If(condition: Expression, expression: Expression, elseExpression: Expression)

  case Tree(expressions: List[Expression])

  case Return(expression: Expression)
  
  case ConstantDeclaration(name: String, expression: Expression)

  case EnumerationDeclaration(name: String, fields: List[String])

  case StructureDeclaration(name: String, fields: List[Parameter])

  case FunctionDeclaration(name: String, tpe: CYType, parameters: List[Parameter], body: Body)

  case ProgramDeclaration(name: String, declarations: List[Expression], body: Body)
}
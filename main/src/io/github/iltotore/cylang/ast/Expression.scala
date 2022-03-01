package io.github.iltotore.cylang.ast

import scala.util.parsing.input.Position
import io.github.iltotore.cylang.eval.EvaluationError
import io.github.iltotore.cylang.{CYType, Context, Parameter, Scope, Variable}

enum Expression(using val position: Position){

  case Empty()(using position: Position)

  case Literal(value: Value)(using position: Position)

  case Negation(expression: Expression)(using position: Position)

  case Addition(left: Expression, right: Expression)(using position: Position)

  case Subtraction(left: Expression, right: Expression)(using position: Position)

  case Multiplication(left: Expression, right: Expression)(using position: Position)

  case Division(left: Expression, right: Expression)(using position: Position)

  case WholeDivision(left: Expression, right: Expression)(using position: Position)

  case Modulo(left: Expression, right: Expression)(using position: Position)

  case Equality(left: Expression, right: Expression)(using position: Position)

  case Greater(left: Expression, right: Expression)(using position: Position)

  case GreaterEqual(left: Expression, right: Expression)(using position: Position)

  case Less(left: Expression, right: Expression)(using position: Position)

  case LessEqual(left: Expression, right: Expression)(using position: Position)

  case Not(expression: Expression)(using position: Position)

  case And(left: Expression, right: Expression)(using position: Position)

  case Or(left: Expression, right: Expression)(using position: Position)

  case VariableCall(name: String)(using position: Position)

  case VariableAssignment(name: String, expression: Expression)(using position: Position)

  case ArrayCall(arrayExpr: Expression, index: Expression)(using position: Position)

  case ArrayAssignment(arrayExpr: Expression, index: Expression, expression: Expression)(using position: Position)

  case StructureCall(structureExpr: Expression, name: String)(using position: Position)

  case StructureAssignment(structureExpr: Expression, name: String, expression: Expression)(using position: Position)

  case FunctionCall(name: String, args: List[Expression])(using position: Position)

  case ForLoop(name: String, from: Expression, to: Expression, step: Expression, expression: Expression)(using position: Position)

  case WhileLoop(condition: Expression, expression: Expression)(using position: Position)

  case DoWhileLoop(condition: Expression, expression: Expression)(using position: Position)

  case If(condition: Expression, expression: Expression, elseExpression: Expression)(using position: Position)

  case Tree(expressions: List[Expression])(using position: Position)

  case Return(expression: Expression)(using position: Position)
  
  case ConstantDeclaration(name: String, expression: Expression)(using position: Position)

  case EnumerationDeclaration(name: String, fields: List[String])(using position: Position)

  case StructureDeclaration(name: String, fields: List[Parameter])(using position: Position)

  case FunctionDeclaration(name: String, tpe: CYType, parameters: List[Parameter], body: Body)(using position: Position)

  case ProgramDeclaration(name: String, declarations: List[Expression], body: Body)(using position: Position)
}
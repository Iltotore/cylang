package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.eval.EvaluationError
import io.github.iltotore.cylang.*

import scala.util.parsing.input.Position

/**
 * Represent each possible node in the AST.
 */
enum Expression(using val position: Position) {

  /**
   * Empty expression.
   */
  case Empty()(using position: Position)

  /**
   * "Not implemented" placeholder.
   */
  case Placeholder()(using position: Position)

  /**
   * A numeric or string literal.
   */
  case Literal(value: Value)(using position: Position)

  /**
   * Equivalent to mathematical negation (e.g -5).
   */
  case Negation(expression: Expression)(using position: Position)

  /**
   * An addition of two expressions.
   */
  case Addition(left: Expression, right: Expression)(using position: Position)

  /**
   * A subtraction of two expressions.
   */
  case Subtraction(left: Expression, right: Expression)(using position: Position)

  /**
   * A multiplication of two expressions.
   */
  case Multiplication(left: Expression, right: Expression)(using position: Position)

  /**
   * A division of two expressions.
   */
  case Division(left: Expression, right: Expression)(using position: Position)

  /**
   * A whole division of two expressions.
   */
  case WholeDivision(left: Expression, right: Expression)(using position: Position)

  /**
   * Mathematical modulo.
   */
  case Modulo(left: Expression, right: Expression)(using position: Position)

  /**
   * Equality test between to expression.
   */
  case Equality(left: Expression, right: Expression)(using position: Position)

  /**
   * Test if the left expression is greater than the right.
   */
  case Greater(left: Expression, right: Expression)(using position: Position)

  /**
   * Test if the left expression is greater than or equal to the right.
   */
  case GreaterEqual(left: Expression, right: Expression)(using position: Position)

  /**
   * Test if the left expression is less than the right.
   */
  case Less(left: Expression, right: Expression)(using position: Position)

  /**
   * Test if the left expression is greater than the right.
   */
  case LessEqual(left: Expression, right: Expression)(using position: Position)

  /**
   * A boolean "not".
   */
  case Not(expression: Expression)(using position: Position)

  /**
   * A boolean "and"
   */
  case And(left: Expression, right: Expression)(using position: Position)

  /**
   * A boolean "or"
   */
  case Or(left: Expression, right: Expression)(using position: Position)

  /**
   * A variable call by its name.
   */
  case VariableCall(name: String)(using position: Position)

  /**
   * A variable (represented by its name) assignment.
   */
  case VariableAssignment(name: String, expression: Expression)(using position: Position)

  /**
   * A "call by index" on another expression.
   */
  case ArrayCall(arrayExpr: Expression, index: Expression)(using position: Position)

  /**
   * An "assignment by index" on another expression.
   */
  case ArrayAssignment(arrayExpr: Expression, index: Expression, expression: Expression)(using position: Position)

  /**
   * A structure field call.
   */
  case StructureCall(structureExpr: Expression, name: String)(using position: Position)

  /**
   * An assignment on a structure field.
   */
  case StructureAssignment(structureExpr: Expression, name: String, expression: Expression)(using position: Position)

  /**
   * A function call by its name.
   */
  case FunctionCall(name: String, args: List[Expression])(using position: Position)

  /**
   * A loop iterating through a numerical range.
   */
  case ForLoop(name: String, from: Expression, to: Expression, step: Expression, expression: Expression)(using position: Position)

  /**
   * A loop iterating as long as the passed condition is true.
   */
  case WhileLoop(condition: Expression, expression: Expression)(using position: Position)

  /**
   * A loop iterating as long as the passed condition is true. Iterate at least once.
   */
  case DoWhileLoop(condition: Expression, expression: Expression)(using position: Position)

  /**
   * Evaluate an expression or another depending on the given condition.
   */
  case IfCondition(condition: Expression, expression: Expression, elseExpression: Expression)(using position: Position)

  /**
   * A list of expressions to execute subsequently.
   */
  case Tree(expressions: List[Expression])(using position: Position)

  /**
   * Return an expression.
   */
  case ReturnExpr(expression: Expression)(using position: Position)

  /**
   * An immutable variable declaration.
   */
  case ConstantDeclaration(name: String, expression: Expression)(using position: Position)

  /**
   * An enumeration declaration.
   */
  case EnumerationDeclaration(name: String, fields: List[String])(using position: Position)

  /**
   * A structure declaration.
   */
  case StructureDeclaration(name: String, fields: List[Parameter])(using position: Position)

  /**
   * A function declaration.
   */
  case FunctionDeclaration(name: String, tpe: CYType, parameters: List[Parameter], body: Body)(using position: Position)

  /**
   * The root of the AST. Represent an entire CYLang program.
   */
  case ProgramDeclaration(name: String, declarations: List[Expression], body: Body)(using position: Position)
}
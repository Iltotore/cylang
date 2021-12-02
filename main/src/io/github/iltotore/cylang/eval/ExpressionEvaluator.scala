package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.{CYType, Context, Variable}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{CYFunction, Expression, Value}

import scala.collection.immutable.NumericRange

trait ExpressionEvaluator {

  given Evaluator[Expression] with {

    extension (input: Expression) {

      override def evaluate(using context: Context): EvalResult = input match {

        case Empty => Right((context, Value.Void))

        case Literal(value) => Right((context, value))

        case Negation(expression) => eval {
          evalUnbox(expression) match {

            case Value.Integer(x) => Value.Integer(-x)

            case Value.Real(x) => Value.Real(-x)

            case _ => ??
          }
        }

        case Addition(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x + y)

            case (Value.Number(x), Value.Number(y)) => Value.Real(x + y)

            case (Value.Text(x), Value(y)) => Value.Text(x + y)

            case (Value(x), Value.Text(y)) => Value.Text(String.valueOf(x) + y)

            case _ => ??
          }
        }

        case Substraction(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x - y)

            case (Value.Number(x), Value.Number(y)) => Value.Real(x - y)

            case _ => ??
          }
        }

        case Multiplication(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x * y)

            case (Value.Number(x), Value.Number(y)) => Value.Real(x * y)

            case _ => ??
          }
        }

        case Division(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(0)) => abort("Division by zero")

            case (Value.Number(x), Value.Number(y)) => Value.Real(x / y)

            case _ => ??
          }
        }

        case WholeDivision(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(0)) => abort("Division by zero")

            case (Value.Number(x), Value.Number(y)) => Value.Integer((x / y).toInt)

            case _ => ??
          }
        }

        case Modulo(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(0)) => abort("Division by zero")

            case (Value.Number(x), Value.Number(y)) => Value.Real(x % y)

            case _ => ??
          }
        }

        case Equality(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(y)) => Value.Bool(x == y)

            case (Value(x), Value(y)) => Value.Bool(x equals y)
          }
        }

        case Greater(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(y)) => Value.Bool(x > y)

            case _ => ??
          }
        }

        case GreaterEqual(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(y)) => Value.Bool(x >= y)

            case _ => ??
          }
        }

        case Less(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(y)) => Value.Bool(x < y)

            case _ => ??
          }
        }

        case LessEqual(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Number(x), Value.Number(y)) => Value.Bool(x <= y)

            case _ => ??
          }
        }

        case Not(expression) => eval {
          evalUnbox(expression) match {

            case Value.Bool(value) => Value.Bool(!value)

            case _ => ??
          }
        }

        case And(left, right) => eval {

          (evalUnbox(left), evalUnbox(right)) match {
            case (Value.Bool(x), Value.Bool(y)) => Value.Bool(x && y)

            case _ => ??
          }
        }

        case Or(left, right) => eval {
          (evalUnbox(left), evalUnbox(right)) match {

            case (Value.Bool(x), Value.Bool(y)) => Value.Bool(x || y)

            case _ => ??
          }
        }

        case VariableCall(name) => context
          .scope
          .variables
          .get(name)
          .map(variable => (context, variable.value))
          .toRight(EvaluationError(s"Unknown variable: $name"))

        case VariableAssignment(name, expression) => eval {
          val variables = currentContext
            .scope
            .variables

          if (variables.contains(name)) {
            update(
              currentContext.copy(
                scope = currentContext.scope.withAssignment(name, evalUnbox(expression))
              )
            )
            Value.Void
          } else abort(s"Unknown variable: $name")
        }

        case FunctionCall(name, args) => eval {
          val function = currentContext.scope.functions.getOrElse(name, abort(s"Unknown function: $name"))
          if (args.length != function.parameters.length)
            abort(s"Invalid argument count. Got: ${args.length}, Expected: ${function.parameters.length}")
          val values = for (arg <- args) yield evalUnbox(arg)
          unbox(function.evaluate(values))
        }

        case ForLoop(name, from, to, step, expression) => eval {
          (evalUnbox(from), evalUnbox(to), evalUnbox(step)) match {

            case (Value.Integer(x), Value.Integer(y), Value.Integer(s)) =>
              update(currentContext.copy(scope = currentContext.scope.withDeclaration(name, CYType.Integer, Value.Integer(x))))
              for (i <- NumericRange(x, y, s)) {
                update(currentContext.copy(scope = currentContext.scope.withAssignment(name, Value.Integer(i))))
                evalUnbox(expression)
              }
              Value.Void

            case _ => ??
          }
        }

        case WhileLoop(condition, expression) => eval {
          while (evalUnbox(condition) match {

            case Value.Bool(x) => x

            case _ => ??
          }) {
            evalUnbox(expression)
          }
          Value.Void
        }

        case If(condition, expression, elseExpression) => eval {
          if (evalUnbox(condition) match {

            case Value.Bool(x) => x

            case _ => ??
          }) evalUnbox(expression)
          else evalUnbox(elseExpression)
          Value.Void
        }

        case Tree(expressions) => eval {
          for (expr <- expressions if currentContext.returned.isEmpty) {
            evalUnbox(expr)
          }
          currentContext.returned.getOrElse(Value.Void)
        }

        case Return(expression) => eval {
          update(currentContext.copy(returned = Some(evalUnbox(expression))))
          Value.Void
        }

        case VariablesDeclaration(variables) =>
          Right((
            variables
              .foldLeft(context)((ctx, variable) => ctx.copy(scope = ctx.scope.withDeclaration(variable._1, variable._2, Value.Void))),
            Value.Void
          ))

        case FunctionDeclaration(name, tpe, parameters, Body(VariablesDeclaration(variables), expression)) =>
          Right((
            context.copy(scope = context.scope.withFunction(
              name,
              CYFunction.UserDefined(tpe, parameters, variables.map { case (k, v) => (k, (v, Value.Void)) }, expression)
            )),
            Value.Void
          ))
      }
    }
  }
}
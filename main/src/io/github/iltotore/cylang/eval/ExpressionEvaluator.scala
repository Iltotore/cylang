package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.{CYType, Context, Cursor, Scope, Variable}
import io.github.iltotore.cylang.ast.Expression.*
import io.github.iltotore.cylang.ast.{Body, CYFunction, Enumeration, Expression, Structure, Value}
import io.github.iltotore.cylang.util.*

import scala.collection.immutable.NumericRange

class ExpressionEvaluator extends Evaluator[Expression] {

  override def evaluateInput(input: Expression)(using context: Context): EvalResult = input match {

    case noCursor: (Tree | ProgramDeclaration) => evaluateNode(noCursor)

    case node =>
      if(context.stack.headOption.forall(!_.function.equals(context.currentFunction))) evaluateNode(node)(using context.copy(stack = context.stack prepended Cursor(context.currentFunction, node.position)))
      else evaluateNode(node)(using context.copy(stack = context.stack.tail prepended Cursor(context.currentFunction, node.position)))
  }

  def evaluateNode(input: Expression)(using context: Context): EvalResult = input match {

    case Empty() => Right((context, Value.Void))

    case Literal(value) => Right((context, value))

    case Negation(expression) => eval {
      evalUnbox(expression) match {

        case Value.Integer(x) => Value.Integer(-x)

        case Value.Real(x) => Value.Real(-x)

        case x => throw EvaluationError.typeMismatch(x)
      }
    }

    case Addition(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x + y)

        case (Value.Number(x), Value.Number(y)) => Value.Real(x + y)

        case (Value.Text(x), Value(y)) => Value.Text(x + y)

        case (Value(x), Value.Text(y)) => Value.Text(String.valueOf(x) + y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x + $y")
      }
    }

    case Subtraction(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x - y)

        case (Value.Number(x), Value.Number(y)) => Value.Real(x - y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x - $y")
      }
    }

    case Multiplication(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Integer(x), Value.Integer(y)) => Value.Integer(x * y)

        case (Value.Number(x), Value.Number(y)) => Value.Real(x * y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x * $y")
      }
    }

    case Division(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(0)) => abort("Division par zero")

        case (Value.Number(x), Value.Number(y)) => Value.Real(x / y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x / $y")
      }
    }

    case WholeDivision(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(0)) => abort("Division par zero")

        case (Value.Number(x), Value.Number(y)) => Value.Integer((x / y).toInt)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x DIV $y")
      }
    }

    case Modulo(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(0)) => abort("Division par zero")

        case (Value.Number(x), Value.Number(y)) => Value.Real(x % y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x MOD $y")
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

        case (x, y) => throw EvaluationError.typeMismatch(s"$x > $y")
      }
    }

    case GreaterEqual(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(y)) => Value.Bool(x >= y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x >= $y")
      }
    }

    case Less(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(y)) => Value.Bool(x < y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x < $y")
      }
    }

    case LessEqual(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Number(x), Value.Number(y)) => Value.Bool(x <= y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x <= $y")
      }
    }

    case Not(expression) => eval {
      evalUnbox(expression) match {

        case Value.Bool(value) => Value.Bool(!value)

        case x => throw EvaluationError.typeMismatch(x)
      }
    }

    case And(left, right) => eval {

      (evalUnbox(left), evalUnbox(right)) match {
        case (Value.Bool(x), Value.Bool(y)) => Value.Bool(x && y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x ET $y")
      }
    }

    case Or(left, right) => eval {
      (evalUnbox(left), evalUnbox(right)) match {

        case (Value.Bool(x), Value.Bool(y)) => Value.Bool(x || y)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x OU $y")
      }
    }

    case VariableCall(name) => context
      .scope
      .variables
      .get(name)
      .map(variable => (context, variable.value))
      .toRight(EvaluationError(s"Variable inconnue: $name"))

    case VariableAssignment(name, expression) => eval {

      update(
        currentContext.copy(
          scope = currentContext.scope.withAssignment(name, evalUnbox(expression)).orThrowLeft
        )
      )
      Value.Void
    }

    case ArrayCall(arrayExpr, index) => eval {
      (evalUnbox(arrayExpr), evalUnbox(index)) match {

        case (Value.Array(values), Value.Integer(i)) =>
          if (values.length <= i) abort(s"L'index $i est en dehors du tableau de taille ${values.length}")
          if (i < 0) abort(s"L'index ne peut pas être négatif ($i)")
          values(i)

        case (x, y) => throw EvaluationError.typeMismatch(s"$x[$y]")
      }
    }

    case ArrayAssignment(arrayExpr, index, expression) => eval {
      (evalUnbox(arrayExpr), evalUnbox(index), evalUnbox(expression)) match {

        case (Value.Array(values), Value.Integer(i), value) =>
          if (values.length <= i) abort(s"L'index $i est en dehors du tableau de taille ${values.length}")
          if (i < 0) abort(s"L'index ne peut pas être négatif ($i)")
          values(i) = value
          Value.Void

        case (x, y, z) => throw EvaluationError.typeMismatch(s"$x[$y] <- $z")
      }
    }

    case StructureCall(structureExpr, name) => eval {
      evalUnbox(structureExpr) match {

        case Value.StructureInstance(structName, fields) =>
          if (!fields.contains(name)) abort(s"$name n'est pas un attribut de la structure $structName")
          fields(name).value

        case x => throw EvaluationError(s"$x n'est pas une structure")
      }
    }

    case StructureAssignment(structureExpr, name, expression) => eval {
      evalUnbox(structureExpr) match {

        case Value.StructureInstance(structName, fields) =>

          if (!fields.contains(name)) abort(s"$name n'est pas un attribut de la structure $structName")
          fields(name) = fields(name).copy(value = evalUnbox(expression))
          Value.Void

        case x => throw EvaluationError(s"$x n'est pas une structure")
      }
    }

    case FunctionCall(name, args) =>
      if(name equals "LIRE") {
        if(args.isEmpty) Left(EvaluationError(s"Nombre d'arguments incorrects pour la fonction $name. Obtenu: ${args.length}, Attendu: 1"))
        else read(args.head).map(ctx => (ctx, Value.Void))
      } else eval {
        val function = currentContext.scope.functions.getOrElse(name, abort(s"Fonction inconnue: $name"))
        if (args.length != function.parameters.length)
          abort(s"Nombre d'arguments incorrects pour la fonction $name. Obtenu: ${args.length}, Attendu: ${function.parameters.length}")
        val values = for (arg <- args) yield evalUnbox(arg)
        val mismatches = values.zip(function.parameters).filterNot(_.tpe isSubTypeOf _.tpe)
        if (mismatches.nonEmpty) throw EvaluationError.typeMismatch(mismatches.map((v, p) => s"$p <- $v").mkString(" et "))
        val globalScope = currentContext
          .scope
          .copy(variables = currentContext.scope.variables.filterNot(_._2.mutable))
        partialUnbox(function.evaluate(values)(using currentContext.copy(scope = globalScope, currentFunction = s"FONCTION $name"), this))._2
      }

    case ForLoop(name, from, to, step, expression) => eval {
      (evalUnbox(from), evalUnbox(to), evalUnbox(step)) match {

        case (Value.Integer(x), Value.Integer(y), Value.Integer(s)) =>
          update(currentContext.copy(scope = currentContext.scope.withAssignment(name, Value.Integer(x)).orThrowLeft))
          for (i <- NumericRange(x, y, s)) {
            update(currentContext.copy(scope = currentContext.scope.withAssignment(name, Value.Integer(i)).orThrowLeft))
            evalUnbox(expression)
          }
          Value.Void

        case (x, y, z) =>
          throw EvaluationError.typeMismatch(s"POUR $name DE $x A $y PAS DE $z")
      }
    }

    case WhileLoop(condition, expression) => eval {
      while (evalUnbox(condition) match {

        case Value.Bool(x) => x

        case x => throw EvaluationError.typeMismatch(x)
      }) {
        evalUnbox(expression)
      }
      Value.Void
    }

    case DoWhileLoop(condition, expression) => eval {
      while {
        evalUnbox(expression)
        evalUnbox(condition) match {

          case Value.Bool(value) => value

          case x => throw EvaluationError.typeMismatch(x)
        }
      } do {}
      Value.Void
    }

    case IfCondition(condition, expression, elseExpression) => eval {
      if (evalUnbox(condition) match {

        case Value.Bool(x) => x

        case x => throw EvaluationError.typeMismatch(x)
      }) evalUnbox(expression)
      else evalUnbox(elseExpression)
      Value.Void
    }

    case Tree(expressions) => eval {
      for (expr <- expressions if currentContext.returned.isEmpty) evalUnbox(expr)
      currentContext.returned.getOrElse(Value.Void)
    }

    case ReturnExpr(expression) => eval {
      update(currentContext.copy(returned = Some(evalUnbox(expression))))
      Value.Void
    }

    case ConstantDeclaration(name, expression) => eval {
      val value = evalUnbox(expression)
      update(
        currentContext.copy(
          scope = currentContext.scope.withDeclaration(name, value.tpe, value, false)
        )
      )
      Value.Void
    }

    case EnumerationDeclaration(name, fields) =>
      Right((
        context.copy(scope = fields
          .foldLeft(context.scope)((scope, field) =>
            scope
              .withDeclaration(field, CYType.EnumerationField(name), Value.EnumerationField(name, field), false)
          )
          .withEnumeration(name, Enumeration(name, fields))
        ),
        Value.Void
      ))

    case StructureDeclaration(name, fields) =>
      Right((
        context.copy(scope = context.scope.withStructure(
          name,
          Structure(name, fields)
        )),
        Value.Void
      ))

    case FunctionDeclaration(name, tpe, parameters, Body(variables, expression)) =>
      Right((
        context.copy(scope = context.scope.withFunction(
          name,
          CYFunction.UserDefined(tpe, parameters, variables.map(param => (param.name, (param.tpe, Value.Void))).toMap, expression)
        )),
        Value.Void
      ))

    case ProgramDeclaration(name, declarations, Body(variables, expression)) => eval {
      declarations.foreach(evalUnbox)
      val scope = variables.foldLeft(currentContext.scope)((scope, param) => param.tpe.defaultValue(using currentContext) match {
        case Right(value) => scope.withDeclaration(param.name, param.tpe, value)
        case Left(err) => throw err
      })
      unbox(expression.evaluate(using currentContext.copy(scope = scope, currentFunction = "PROGRAMME PRINCIPAL")))
    }
  }
}

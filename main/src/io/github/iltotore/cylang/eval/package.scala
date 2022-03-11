package io.github.iltotore.cylang

import scala.util.{Failure, Success, Try}
import io.github.iltotore.cylang.ast.{Expression, Value}
import io.github.iltotore.cylang.ast.Expression.VariableCall
import io.github.iltotore.cylang.CYType

import java.util.Scanner
import scala.io.StdIn

package object eval {

  type EvalResult = Either[EvaluationError, (Context, Value)]

  type Evaluation = Context ?=> EvalResult

  given Evaluator[Expression] = new ExpressionEvaluator

  def read(expr: Expression)(using context: Context): Either[EvaluationError, Value] = expr match {

    case VariableCall(name) if context.scope.variables.contains(name) =>

      val scanner = Scanner(context.in)

      context.scope.variables(name).tpe match {

        case CYType.Boolean =>
          scanner
            .nextLine()
            .toBooleanOption
            .map(Value.Bool.apply)
            .toRight(EvaluationError("La valeur saisie n'est pas un booléen !"))

        case CYType.Integer =>
          scanner
            .nextLine()
            .toIntOption
            .map(Value.Integer.apply)
            .toRight(EvaluationError("La valeur saisie n'est pas un entier !"))

        case CYType.Real =>
          scanner
            .nextLine()
            .toDoubleOption
            .map(Value.Real.apply)
            .toRight(EvaluationError("La valeur saisie n'est pas un réel !"))

        case CYType.Character =>
          Right(Value.Character(scanner.nextLine().charAt(0)))

        case CYType.Text => Right(Value.Text(scanner.nextLine()))

        case tpe => Left(EvaluationError(s"Impossible de lire le type $tpe directement."))
      }

    case _ => Left(EvaluationError("LIRE doit prendre une variable en paramètre"))
  }
}
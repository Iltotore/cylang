package io.github.iltotore.cylang

import io.github.iltotore.cylang.CYType
import io.github.iltotore.cylang.ast.Expression.VariableCall
import io.github.iltotore.cylang.ast.{Expression, Value}

import java.io.{BufferedReader, InputStreamReader}
import java.util.Scanner
import scala.io.StdIn
import scala.util.{Failure, Success, Try}
import io.github.iltotore.cylang.util.LineReader

package object eval {

  /**
   * Represent the result of an AST evaluation.
   */
  type EvalResult = Either[EvaluationError, (Context, Value)]

  given Evaluator[Expression] = new ExpressionEvaluator

  /**
   * Implementation of the `LIRE` function. Wait then read the next input.
   *
   * @param expr    the expression passed as argument
   * @param context the current evaluation context
   * @return the updated context or an error
   */
  def read(expr: Expression)(using context: Context): Either[EvaluationError, Context] = {

    def readLine(reader: LineReader): String = {
      var line = reader.readLine()
      while (line == null) line = reader.readLine()
      line
    }

    expr match {

      case VariableCall(name) if context.scope.variables.contains(name) =>

        val reader = context.in

        val value = context.scope.variables(name).tpe match {

          case CYType.Boolean =>

            readLine(reader)
              .toBooleanOption
              .map(Value.Bool.apply)
              .toRight(EvaluationError("La valeur saisie n'est pas un booléen !"))

          case CYType.Integer =>
            readLine(reader)
              .toIntOption
              .map(Value.Integer.apply)
              .toRight(EvaluationError("La valeur saisie n'est pas un entier !"))

          case CYType.Real =>
            readLine(reader)
              .toDoubleOption
              .map(Value.Real.apply)
              .toRight(EvaluationError("La valeur saisie n'est pas un réel !"))

          case CYType.Character =>
            Right(Value.Character(readLine(reader).charAt(0)))

          case CYType.Text => Right(Value.Text(readLine(reader)))

          case tpe => Left(EvaluationError(s"Impossible de lire le type $tpe directement."))
        }

        for {
          x <- value
          scope <- context.scope.withAssignment(name, x)
        } yield context.copy(scope = scope)

      case _ => Left(EvaluationError("LIRE doit prendre une variable en paramètre"))
    }
  }
}
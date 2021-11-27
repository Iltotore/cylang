package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.Context
import io.github.iltotore.cylang.ast.{Expression, Value}
import io.github.iltotore.cylang.ast.Expression._
import io.github.iltotore.cylang.Context
import io.github.iltotore.cylang.util._

trait Evaluator[-A] {

  extension (input: A) {

    def evaluate(using Context): EvalResult
  }
}
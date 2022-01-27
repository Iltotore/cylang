package io.github.iltotore.cylang.eval

import io.github.iltotore.cylang.Context

trait Evaluator[-A] {
  
    def evaluate(input: A)(using Context): EvalResult
}
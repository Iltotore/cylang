package io.github.iltotore

import io.github.iltotore.cylang.eval.EvaluationError

package object cylang {

  type Result[T] = Either[EvaluationError, T]
  
}

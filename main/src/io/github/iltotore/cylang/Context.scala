package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

case class Context(scope: Scope, stack: List[Int], returned: Option[Value])

object Context {
  
  val empty: Context = Context(Scope.empty, List.empty, None)
}

package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

case class Context(scope: Scope, stack: List[Int], returned: Option[Value]) {
  
  def merged(context: Context): Context = this.copy(scope = this.scope.merged(context.scope), returned = context.returned)
}

object Context {
  
  val empty: Context = Context(Scope.empty, List.empty, None)
}

package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

import java.io.{InputStream, PrintStream}

case class Context(in: InputStream, out: PrintStream, scope: Scope, currentFunction: String, stack: List[Cursor], returned: Option[Value])

object Context {
  
  val empty: Context = Context(Scope.empty, "en-tête du programme", List.empty, None)
}

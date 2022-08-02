package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

import java.io.{InputStream, PrintStream}

/**
 * An evaluation context.
 * @param in the input stream/user input
 * @param out the output stream/console output
 * @param scope the current variable/function scope
 * @param currentFunction the function being evaluated
 * @param stack the current evaluation stack
 * @param returned the (optionally) returned value
 */
case class Context(in: InputStream, out: PrintStream, scope: Scope, currentFunction: String, stack: List[Cursor], returned: Option[Value])

object Context {

  /**
   * An empty context using System's I/O.
   */
  val empty: Context = Context(System.in, System.out, Scope.default, "en-tête du programme", List.empty, None)
}

package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

import java.io.{InputStream, PrintStream}
import io.github.iltotore.cylang.util.LineReader

/**
 * An evaluation context.
 *
 * @param in              the input stream/user input
 * @param out             the output stream/console output
 * @param scope           the current variable/function scope
 * @param currentFunction the function being evaluated
 * @param stack           the current evaluation stack
 * @param returned        the (optionally) returned value
 * @param maxDepth        the maximum depth before throwing a stack overflow error
 */
case class Context(
  in: LineReader,
  out: PrintStream, 
  scope: Scope,
  currentFunction: String,
  stack: List[Cursor],
  returned: Option[Value],
  maxDepth: Int
)

object Context {

  /**
   * An empty context using System's I/O.
   */
  val empty: Context = Context(LineReader.fromInputStream(System.in), System.out, Scope.default, "en-tête du programme", List.empty, None, 100)
}

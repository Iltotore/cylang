package io.github.iltotore.cylang

import scala.util.parsing.input.Position

/**
 * A location in the AST.
 *
 * @param function the name of the function where the cursor is
 * @param position the position where the cursor is
 */
case class Cursor(function: String, position: Position) {

  lazy val trace: String = s"Vers la ligne ${position.line}, colonne ${position.column}, dans $function"
}

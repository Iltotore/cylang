package io.github.iltotore.cylang

import scala.util.parsing.input.Position

/**
 * An absolute position.
 *
 * @param line         the line of this position
 * @param column       the column of this position
 * @param lineContents the content of the line where this position is
 */
case class FixedPosition(line: Int, column: Int, lineContents: String) extends Position

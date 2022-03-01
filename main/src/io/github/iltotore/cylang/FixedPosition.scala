package io.github.iltotore.cylang

import scala.util.parsing.input.Position

case class FixedPosition(line: Int, column: Int, lineContents: String) extends Position

package io.github.iltotore.cylang

import scala.util.parsing.input.Position

case class Cursor(function: String, position: Position) {

  lazy val trace: String = s"Vers la ligne ${position.line}, colonne ${position.column}, dans $function"
}

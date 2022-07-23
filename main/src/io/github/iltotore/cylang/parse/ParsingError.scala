package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.CYError

import scala.util.parsing.input.{NoPosition, Position}

case class ParsingError(msg: String, position: Position = NoPosition) extends Error(msg) with CYError {

  override def toString: String =
    if (position equals NoPosition) s"Erreur de syntaxe: $msg"
    else
      s"""Erreur de syntaxe: $msg
         |
         |Vers la ligne ${position.line}, colonne ${position.column}
         |
         |${position.longString}""".stripMargin
}

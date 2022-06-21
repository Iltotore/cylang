package io.github.iltotore.cylang.parse

import io.github.iltotore.cylang.CYError

case class ParsingError(msg: String) extends Error(msg) with CYError {

  override def toString: String = msg
}

package io.github.iltotore.cylang

/**
 * Represent a function parameter. Note that variable declarations also are parameters.
 * @param name the name of this parameter
 * @param tpe the expected type of this parameter
 */
case class Parameter(name: String, tpe: CYType) {

  override def toString: String = s"($name: $tpe)"
}
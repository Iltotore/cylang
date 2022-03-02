package io.github.iltotore.cylang

case class Parameter(name: String, tpe: CYType) {

  override def toString: String = s"($name: $tpe)"
}
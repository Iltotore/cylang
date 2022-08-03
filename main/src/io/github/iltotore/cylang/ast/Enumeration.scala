package io.github.iltotore.cylang.ast

/**
 * Represent a declared enumeration.
 *
 * @param name   the name of this enumeration
 * @param fields the constant fields of this expression
 */
case class Enumeration(name: String, fields: List[String])

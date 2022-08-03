package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.Parameter

/**
 * The body of a function or program.
 *
 * @param variables  the declared variables of this function or program
 * @param expression the expression executed when calling this function or program
 */
case class Body(variables: List[Parameter], expression: Expression)

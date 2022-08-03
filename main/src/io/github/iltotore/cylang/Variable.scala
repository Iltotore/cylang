package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

/**
 * A variable or constant.
 *
 * @param tpe     the type of this variable
 * @param value   the current value of this variable
 * @param mutable whether this variable is mutable or not
 */
case class Variable(tpe: CYType, value: Value, mutable: Boolean)
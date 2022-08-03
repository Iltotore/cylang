package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.Parameter

/**
 * A structure with its fields.
 *
 * @param name   the name of this structure.
 * @param fields the fields/variables of this structure.
 */
case class Structure(name: String, fields: List[Parameter])
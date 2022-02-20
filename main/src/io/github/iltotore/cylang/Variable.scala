package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Value

case class Variable(tpe: CYType, value: Value, mutable: Boolean)
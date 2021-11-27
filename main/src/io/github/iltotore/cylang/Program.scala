package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.Expression

case class Program(name: String, variables: List[Variable], expression: Expression)

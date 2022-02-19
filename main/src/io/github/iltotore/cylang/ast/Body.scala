package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.Parameter

case class Body(variables: List[Parameter], expression: Expression)

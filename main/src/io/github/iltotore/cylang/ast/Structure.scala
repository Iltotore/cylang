package io.github.iltotore.cylang.ast

import io.github.iltotore.cylang.Parameter

case class Structure(name: String, fields: List[Parameter])
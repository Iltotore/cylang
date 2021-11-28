package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.CYFunction

case class Root(program: Program, functions: List[CYFunction])

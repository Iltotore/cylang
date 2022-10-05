package io.github.iltotore.cylang.editor

import io.github.iltotore.cylang.*

enum Msg {
  case LoadPredef(result: Either[CYError, Context])
  case EditCode(code: String)
  case Run
  case Finish(error: Option[CYError])
  case Download
  case Clear
  case Print(message: String)
}
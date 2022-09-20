package io.github.iltotore.cylang.editor

import cats.effect.IO
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*

@JSExportTopLevel("TyrianApp")
object Main extends TyrianApp[Msg, Model] {

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) =
    ((), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) = {
    _ => ((), Cmd.None)
  }

  def view(model: Model): Html[Msg] =
    div(
        styles(
          ("top", "0"),
          ("left", "0"),
          ("margin", "0"),
          ("padding", "0"),
          ("width", "100%"),
          ("height", "90vh"),
          ("max-height", "90vh"),
          ("width", "100vw"),
          ("max-width", "100vw")
        )
    )(
      div(
        button()("Exécuter"),
        button()("Télécharger"),
        button()("Effacer la console")
      ),
      textarea(
        spellcheck := false,
        styles(
          ("margin", "0"),
          ("padding", "0"),
          ("resize", "none"),
          ("width", "100%"),
          ("height", "70%"),
          ("border", "none")
        )
      )(),
      div(
        styles(
          ("margin", "0"),
          ("padding", "0"),
          ("resize", "height"),
          ("width", "100%"),
          ("height", "30%"),
          ("white-space", "pre-wrap"),
          ("overflow", "auto")
        )
      )("Hello World\na\na\na\na\na\na\na\na\na\na\na\na\na\na")
    )

  def subscriptions(model: Model): Sub[IO, Msg] =
    Sub.None
}

type Model = Unit

enum Msg {
  case Stub
}

package io.github.iltotore.cylang.editor

import scala.io.Source
import scala.util.Try
import cats.effect.IO
import tyrian.Html.*
import tyrian.*

import scala.scalajs.js.annotation.*
import org.scalajs.dom

import io.github.iltotore.cylang.*
import io.github.iltotore.cylang.eval.given
import io.github.iltotore.cylang.editor.util.readTextFile

@JSExportTopLevel("TyrianApp")
object Main extends TyrianApp[Msg, EditorModel] {

  def init(flags: Map[String, String]): (EditorModel, Cmd[IO, Msg]) = //TODO Use JS' FileReader
    val contextIO =
      for {
        stdSrc <- readTextFile("predef.cy")
        stdRes <- IO.fromEither(execute(stdSrc)(using Context.empty))
      } yield stdRes._1

    (EditorModel(Context.empty), Cmd.Run(contextIO, Msg.SwitchContext.apply))

  def update(model: EditorModel): Msg => (EditorModel, Cmd[IO, Msg]) = {

    case Msg.SwitchContext(ctx) =>
      println(ctx)
      (model.copy(context = ctx), Cmd.None)

    case _ => (model, Cmd.None)
  }

  def view(model: EditorModel): Html[Msg] =
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

  def subscriptions(model: EditorModel): Sub[IO, Msg] =
    Sub.None
}


enum Msg {
  case SwitchContext(context: Context)
  case Run
  case Download
  case Clean
  case Print
}

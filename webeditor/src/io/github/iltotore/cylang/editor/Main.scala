package io.github.iltotore.cylang.editor

import cats.effect.IO
import io.github.iltotore.cylang.*
import io.github.iltotore.cylang.editor.util.{readInput, readTextFile, subFromStream}
import io.github.iltotore.cylang.eval.given
import org.scalajs.dom
import tyrian.*
import tyrian.Html.*

import java.io.{PipedInputStream, PipedOutputStream, PrintStream}
import scala.io.Source
import scala.scalajs.js.annotation.*
import scala.util.Try

@JSExportTopLevel("TyrianApp")
object Main extends TyrianApp[Msg, EditorModel] {

  def init(flags: Map[String, String]): (EditorModel, Cmd[IO, Msg]) =

    val pipe = PipedOutputStream()

    val loadPredef =
      for {
        stdSrc <- readTextFile("predef.cy")
        stdRes <- runCode(stdSrc, Context.empty.copy(out = PrintStream(pipe)))
      } yield stdRes

    (EditorModel.empty(pipe), Cmd.Run(loadPredef, Msg.LoadPredef.apply))

  def update(model: EditorModel): Msg => (EditorModel, Cmd[IO, Msg]) = {

    case Msg.LoadPredef(Right(ctx)) =>
      (model.copy(context = ctx, output = "Bibliothèque standard chargée"), Cmd.None)

    case Msg.LoadPredef(Left(error)) => (model.copy(output = error.toString), Cmd.None)

    case Msg.EditCode(code) => (model.copy(currentCode = code), Cmd.None)

    case Msg.Run =>
      (model.copy(output = "Lancement..."), Cmd.Run(runCode(model.currentCode, model.context), result => Msg.Finish(result.left.toOption)))

    case Msg.Clear => (model.copy(output = ""), Cmd.None)

    case Msg.Finish(error) =>
      val result = error.map(_.toString).getOrElse("Exécution terminée")
      (model.copy(output = s"${model.output}\n$result"), Cmd.None)

    case Msg.Print(msg) =>
      print(msg)
      (model.copy(output = s"${model.output}$msg"), Cmd.None)

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
        button(onClick(Msg.Run))("Exécuter"),
        button()("Télécharger"),
        button(onClick(Msg.Clear))("Effacer la console")
      ),
      textarea(
        spellcheck := false,
        onInput(Msg.EditCode.apply),
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
      )(model.output)
    )

  def subscriptions(model: EditorModel): Sub[IO, Msg] =
    subFromStream("outPipe", readInput(PipedInputStream(model.outPipe))).map(Msg.Print.apply)

  private def runCode(code: String, context: Context): IO[Either[CYError, Context]] =
    IO(execute(code)(using context).map(_._1))

}


enum Msg {
  case LoadPredef(result: Either[CYError, Context])
  case EditCode(code: String)
  case Run
  case Finish(error: Option[CYError])
  case Download
  case Clear
  case Print(message: String)
}

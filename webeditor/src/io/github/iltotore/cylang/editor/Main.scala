package io.github.iltotore.cylang.editor

import cats.effect.IO
import io.github.iltotore.cylang.*
import io.github.iltotore.cylang.editor.util.*
import io.github.iltotore.cylang.eval.given
import org.scalajs.dom
import tyrian.*
import tyrian.Html.*

import java.io.PrintStream
import scala.concurrent.duration.given
import scala.io.Source
import scala.scalajs.js.Dynamic
import scala.scalajs.js.annotation.*
import scala.util.Try

@JSExportTopLevel("TyrianApp")
object Main extends TyrianApp[Msg, EditorModel] {

  def init(flags: Map[String, String]): (EditorModel, Cmd[IO, Msg]) =

    val pipe = PullableOutputStream()
    val printStream = PrintStream(pipe)

    val loadPredef =
      for {
        stdSrc <- readTextFile("predef.cy")
        stdRes <- runCode(stdSrc, Context.empty.copy(in = PromptReader(), out = printStream, maxDepth = 15))
      } yield stdRes

    (EditorModel.empty(pipe), Cmd.Run(loadPredef, Msg.LoadPredef.apply))

  def update(model: EditorModel): Msg => (EditorModel, Cmd[IO, Msg]) = {

    case Msg.LoadPredef(Right(ctx)) =>
      (model.copy(context = ctx, output = "Bibliothèque standard chargée"), Cmd.None)

    case Msg.LoadPredef(Left(error)) => (model.copy(output = error.toString), Cmd.None)

    case Msg.EditCode(code) =>
      println("edited code")
      (model.copy(currentCode = code), Cmd.None)

    case Msg.Run =>
      (model.copy(output = "Lancement...\n"), Cmd.Run(runCode(model.currentCode, model.context), result => Msg.Finish(result.left.toOption)))

    case Msg.Download =>
      generateFile("code.cy", model.currentCode)
      (model, Cmd.None)

    case Msg.Clear => (model.copy(output = ""), Cmd.None)

    case Msg.Finish(error) =>
      val result = error.map(_.toString).getOrElse("Exécution terminée")
      model.context.out.println(result)
      (model, Cmd.Run(fetchConsoleOutput(model.outPipe), Msg.Print.apply))

    case Msg.Print(msg) =>
      print(msg)
      (model.copy(output = s"${model.output}$msg"), Cmd.None)
  }

  def view(model: EditorModel): Html[Msg] =
    div(
      id := "fulldiv",
      className := "noborder"
    )(
      div(id := "toolbar")(
        div(id := "buttons")(
          button(className := "button", onClick(Msg.Run))("Exécuter"),
          button(className := "button", onClick(Msg.Download))("Télécharger"),
          button(className := "button", onClick(Msg.Clear))("Effacer la console")
        ),
        a(id := "social", href := "https://github.com/Iltotore/cylang", target := "_blank")(
          img(className := "socialLogo", src := "github_icon.svg"),
          span(className := "socialText")("CY Lang créé par Raphaël FROMENTIN")
        )
      ),
      gui.editor(model.currentCode.count(_ == '\n')),
      div(
        id := "console",
        className := "noborder"
      )(model.output)
    )

  def subscriptions(model: EditorModel): Sub[IO, Msg] = Sub.None

  private def runCode(code: String, context: Context): IO[Either[CYError, Context]] =
    IO.blocking(execute(code)(using context).map(_._1))

  def fetchConsoleOutput(out: PullableOutputStream): IO[String] =
    IO(out.dequeueAllData).map(seq => String(seq.toArray))

}
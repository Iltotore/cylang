package io.github.iltotore.cylang.editor

import tyrian.*
import tyrian.Html.*
import org.scalajs.dom.HTMLElement
import tyrian.Tyrian.HTMLInputElement

object gui {

  def lineIndex(count: Int): Html[Msg] =
      label(id := "line-index")(
          Range.inclusive(1, count+1).mkString("\n")
      )

  def editor(lineCount: Int): Html[Msg] = {
    val h = (lineCount+1.75)*1.9
    val padding = 4

    div(id := "editor")(
      lineIndex(lineCount),
      textarea(
          id := "editor-code",
          className := "noborder",
          spellcheck := false,
          style("min-height", s"calc(${h}ch + ${padding}px)"),
          onInput(Msg.EditCode.apply)
      )()
    )
  }
}
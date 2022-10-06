package io.github.iltotore.cylang.editor

import tyrian.*
import tyrian.Html.*

object gui {

    def lineIndex(count: Int): Html[Msg] =
        label(id := "line-index")(
            Range.inclusive(1, count+1).mkString("\n")
        )

    def editor(lineCount: Int): Html[Msg] =
      div(id := "editor")(
        lineIndex(lineCount),
        textarea(
            id := "editor-code",
            className := "noborder",
            spellcheck := false,
            onInput(Msg.EditCode.apply)
        )()
      )
}
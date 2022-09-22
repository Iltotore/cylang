package io.github.iltotore.cylang.editor

import io.github.iltotore.cylang.Context

case class EditorModel(context: Context, currentCode: String, output: String)

object EditorModel {
  
  val empty: EditorModel = EditorModel(Context.empty, "", "")
}
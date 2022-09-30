package io.github.iltotore.cylang.editor

import io.github.iltotore.cylang.Context

case class EditorModel(context: Context, currentCode: String, output: String, outPipe: PullableOutputStream)

object EditorModel {
  
  def empty(outPipe: PullableOutputStream): EditorModel = EditorModel(Context.empty, "", "", outPipe)
}
package io.github.iltotore.cylang.editor

import io.github.iltotore.cylang.Context

import java.io.PipedOutputStream

case class EditorModel(context: Context, currentCode: String, output: String, outPipe: PipedOutputStream)

object EditorModel {
  
  def empty(outPipe: PipedOutputStream): EditorModel = EditorModel(Context.empty, "", "", outPipe)
}
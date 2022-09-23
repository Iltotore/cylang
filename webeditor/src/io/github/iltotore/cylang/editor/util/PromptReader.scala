package io.github.iltotore.cylang.editor

import scala.scalajs.js.Dynamic

import io.github.iltotore.cylang.util.LineReader

class PromptReader extends LineReader {

    override def readLine(): String = Dynamic.global.prompt("Entrez une valeur", "").asInstanceOf[String]
}
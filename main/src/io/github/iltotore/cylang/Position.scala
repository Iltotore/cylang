package io.github.iltotore.cylang

case class Position(line: Int, column: Int, lineSource: String) {

  lazy val content: String =
    s"""$lineSource
       |${lineSource.take(column - 1).map { x => if (x == '\t') x else ' ' }} ^""".stripMargin
}

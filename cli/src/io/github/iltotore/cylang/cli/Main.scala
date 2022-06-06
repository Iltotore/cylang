package io.github.iltotore.cylang.cli

import io.github.iltotore.cylang.{Context, execute}
import io.github.iltotore.cylang.eval.given

import java.io.File
import scala.io.{AnsiColor, Source}
import scala.util.Try

object Main {

  def main(args: Array[String]): Unit = {

    val result = for {
      fileName <- Either.cond(args.length == 1, args(0), "Usage: cylang <fichier>")
      file <- Right(File(System.getProperty("user.dir"), fileName))
        .filterOrElse(_.exists, "Le fichier spécifié est introuvable")
      src <- Try(Source.fromFile(file)).toEither.left.map(_.toString)
      stdSrc <- Try(Source.fromInputStream(getClass.getResourceAsStream("/predef.cy"))).toEither.left.map(_.toString)
      stdRes <- execute(stdSrc.mkString)(using Context.empty).left.map(_.toString)
      _ <- execute(src.mkString)(using stdRes._1).map(_.toString)
    } yield ()

    result.left.foreach(err => println(s"${AnsiColor.RED}$err${AnsiColor.RESET}"))
  }
}

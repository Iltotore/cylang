package io.github.iltotore.cylang.cli

import io.github.iltotore.cylang.eval.given
import io.github.iltotore.cylang.{Context, execute}

import java.io.File
import java.util.jar.{Attributes, Manifest}
import scala.io.{AnsiColor, Source}
import scala.util.{Properties, Try}

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      val arg = args(0)
      if (arg.equals("--version")) printVersion()
      else if(arg.startsWith("-")) printUsage()
      else runFile(arg)
    } else printUsage()
  }

  def printUsage(): Unit = println("Usage: cylang <fichier> OU cylang --version")

  def printVersion(): Unit = {
    val versions =
      Source
        .fromInputStream(getClass.getResourceAsStream("/versions.properties"))
        .mkString
        .replace("=", ": v")

    println(
      s"""$versions
         |
         |Créé par Raphaël Fromentin""".stripMargin
    )
  }

  def runFile(fileName: String): Unit = {
    val result = for {
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

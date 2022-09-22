package io.github.iltotore.cylang.editor

import scala.concurrent.{Future, Promise}

import scala.scalajs.js.Any.{*, given}
import scala.scalajs.js
import org.scalajs.dom.*

import cats.effect.IO

object util {

    val fs = js.Dynamic.global.fetch
  
    def readTextFile(file: String): IO[String] =
        IO
        .fromPromise(IO(fetch(file).`then`(_.text())))
}

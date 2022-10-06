package io.github.iltotore.cylang.editor

import scala.concurrent.{Future, Promise}

import scala.scalajs.js.Any.{*, given}
import scala.scalajs.js
import org.scalajs.dom.*

import java.io.InputStream

import cats.effect.*

import fs2.Stream

import tyrian.Sub

import js.Dynamic.global
import js.Dynamic

package object util {

    val fs = global.fetch

    def readTextFile(file: String): IO[String] =
        IO
        .fromPromise(IO(fetch(file).`then`(_.text())))

    def readInput(input: InputStream): Stream[IO, String] =
        fs2.io.readInputStream(IO(System.in), 1024, false)
          .through(fs2.text.utf8.decode)

    def generateFile(name: String, text: String): Unit = {
        val element = global.document.createElement("a")
        element.setAttribute("href", s"data:text/plain;charset=utf-8,${global.encodeURIComponent(text)}")
        element.setAttribute("download", name)

        element.style.display = "none"
        document.body.appendChild(element.asInstanceOf[Node])

        element.click()

        document.body.removeChild(element.asInstanceOf[Node])
    }
}

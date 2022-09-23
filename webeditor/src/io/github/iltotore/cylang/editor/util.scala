package io.github.iltotore.cylang.editor

import scala.concurrent.{Future, Promise}

import scala.scalajs.js.Any.{*, given}
import scala.scalajs.js
import org.scalajs.dom.*

import java.io.InputStream

import cats.effect.*

import fs2.Stream

import tyrian.Sub

object util {

    val fs = js.Dynamic.global.fetch
    def readTextFile(file: String): IO[String] =
        IO
        .fromPromise(IO(fetch(file).`then`(_.text())))

    def readInput(input: InputStream): Stream[IO, String] =
        fs2.io.readInputStream(IO(System.in), 1024, false)
          .through(fs2.text.utf8.decode)

    def subFromStream[F[_]: Async, A](id: String, stream: Stream[F, A]): Sub[F, A] =
        Sub.make[F, A, Fiber[F, Throwable, Unit]](id) { cb =>
            Async[F].start(stream.attempt.foreach(result => Async[F].delay(cb(result))).compile.drain)
        }(_.cancel)

}

package io.github.iltotore.cylang.util

import javax.sound.sampled.Line
import java.io.{BufferedReader, InputStream, InputStreamReader}

trait LineReader {

    def readLine(): String
}

object LineReader {

    class BufferedInput(input: BufferedReader) extends LineReader {

        override def readLine(): String = input.readLine()
    }

    def fromInputStream(input: InputStream): LineReader = BufferedInput(BufferedReader(InputStreamReader(input)))
}
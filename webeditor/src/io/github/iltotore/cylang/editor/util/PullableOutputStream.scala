package io.github.iltotore.cylang.editor

import scala.collection.mutable.Queue

import java.io.OutputStream

class PullableOutputStream extends OutputStream {

    private val queue: Queue[Int] = Queue.empty

    override def write(b: Int): Unit = queue.enqueue(b)

    def dequeue: Option[Int] = queue.dequeueFirst(_ => true)

    def dequeueData: Option[Byte] = queue.dequeueFirst(_ != -1).map(_.toByte)

    def dequeueAllData: Seq[Byte] = queue.dequeueAll(_ != -1).map(_.toByte) 
}
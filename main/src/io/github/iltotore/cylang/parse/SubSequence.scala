package io.github.iltotore.cylang.parse

import scala.util.parsing.combinator.SubSequence

class SubSequence(s: CharSequence, start: Int, val length: Int) extends CharSequence {
  def this(s: CharSequence, start: Int) = this(s, start, s.length - start)

  def charAt(i: Int): Char =
    if (i >= 0 && i < length) s.charAt(start + i) else throw new IndexOutOfBoundsException(s"index: $i, length: $length")

  def subSequence(_start: Int, _end: Int): SubSequence = {
    if (_start < 0 || _end < 0 || _end > length || _start > _end)
      throw new IndexOutOfBoundsException(s"start: ${_start}, end: ${_end}, length: $length")

    new SubSequence(s, start + _start, _end - _start)
  }

  override def toString: String = s.subSequence(start, start + length).toString
}

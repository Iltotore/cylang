package io.github.iltotore.cylang.ast

import scala.collection.mutable
import io.github.iltotore.cylang.{CYType, Variable}

sealed trait Value {

  def tpe: CYType

  def value: Any

  override def toString: String = s"($value: $tpe)"
}

object Value {

  def unapply(value: Value): Tuple1[Any] = Tuple1(value.value)

  trait Number extends Value {

    def toDouble: Double
  }

  object Number {

    def unapply(number: Number): Tuple1[Double] = Tuple1(number.toDouble)
  }

  case class Integer(value: Int) extends Number {

    override val tpe: CYType = CYType.Integer

    override def toDouble: Double = value
  }

  case class Real(value: Double) extends Number {

    override val tpe: CYType = CYType.Real

    override def toDouble: Double = value
  }

  case class Character(value: Char) extends Value {

    override val tpe: CYType = CYType.Character
  }

  case class Text(value: String) extends Value {

    override val tpe: CYType = CYType.Text
  }
  
  case class Bool(value: Boolean) extends Value {

    override def tpe: CYType = CYType.Boolean
  }
  
  case class Array(value: scala.Array[Value]) extends Value {

    override def tpe: CYType = CYType.Array(value.headOption.fold(CYType.Void)(_.tpe), Some(value.length))
  }

  case class EnumerationField(name: String, value: String) extends Value {

    override def tpe: CYType = CYType.EnumerationField(name)
  }

  case class StructureInstance(name: String, value: mutable.Map[String, Variable]) extends Value {

    override def tpe: CYType = CYType.StructureInstance(name)
  }

  case object Void extends Value {

    override def tpe: CYType = CYType.Void

    override val value: Null = null
  }
}
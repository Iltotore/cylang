package io.github.iltotore.cylang

import ast.Structure

sealed trait CYType {
  
  def name: String
}

object CYType {
  
  sealed trait Number extends CYType
  
  case object Integer extends Number {

    override val name: String = "entier"
  }

  case object Real extends Number {

    override def name: String = "reel"
  }

  case object Character extends CYType {

    override def name: String = "caractere"
  }

  case object Text extends CYType {

    override def name: String = "string"
  }

  case object Boolean extends CYType {

    override def name: String = "booleen"
  }

  case class Array(innerType: CYType, size: Option[Int]) extends CYType {

    override def name: String = s"tableau de $innerType de taille ${size.getOrElse("inconnue")}"
  }
  
  case class StructureInstance(structure: Structure) extends CYType {

    override def name: String = s"structure ${structure.name}"
  }
  
  case object Void extends CYType {

    override def name: String = "void"
  }

  case object Unknown extends CYType {

    override def name: String = "unknown"
  }

  val rawTypes: List[CYType] = List(CYType.Integer, CYType.Real, CYType.Character, CYType.Text, CYType.Boolean)
}
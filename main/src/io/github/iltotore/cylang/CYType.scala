package io.github.iltotore.cylang

sealed trait CYType {
  
  def name: String
}

object CYType {
  
  sealed trait Number extends CYType
  
  case object Integer extends Number {

    override val name: String = "integer"
  }

  case object Real extends Number {

    override def name: String = "real"
  }

  case object Character extends CYType {

    override def name: String = "char"
  }

  case object Text extends CYType {

    override def name: String = "string"
  }

  case object Boolean extends CYType {

    override def name: String = "boolean"
  }
  
  case object Void extends CYType {

    override def name: String = "void"
  }

  case object Unknown extends CYType {

    override def name: String = "unknown"
  }
}
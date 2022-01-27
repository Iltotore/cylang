package io.github.iltotore.cylang

import scala.collection.mutable
import ast.Structure
import ast.Value
import io.github.iltotore.cylang.eval.EvaluationError
import io.github.iltotore.cylang.util.*

sealed trait CYType {

  def name: String

  def defaultValue(using Context): Either[EvaluationError, Value]
}

object CYType {

  sealed trait Number extends CYType

  case object Integer extends Number {

    override val name: String = "entier"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Integer(0))
  }

  case object Real extends Number {

    override def name: String = "reel"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Real(0))
  }

  case object Character extends CYType {

    override def name: String = "caractere"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Character(0.toChar))
  }

  case object Text extends CYType {

    override def name: String = "string"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Text(""))
  }

  case object Boolean extends CYType {

    override def name: String = "booleen"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Bool(true))
  }

  case class Array(innerType: CYType, size: Option[Int]) extends CYType {

    override def name: String = s"tableau de $innerType de taille ${size.getOrElse("inconnue")}"

    override def defaultValue(using Context): Either[EvaluationError, Value] = either {
      if(size.isEmpty) left(EvaluationError("Array variables must have a defined size"))
      val values = for(i <- 0 until size.get) yield ensureRight(innerType.defaultValue)
      Value.Array(values.toArray)
    }
  }

  case class StructureInstance(structName: String) extends CYType {

    override def name: String = s"structure $structName"

    override def defaultValue(using context: Context): Either[EvaluationError, Value] = either {
      val structure = ensureOption(context.scope.structures.get(structName))(EvaluationError(s"Unknown structure $structName"))
      val values = for(field <- structure.fields) yield (field.name, Variable(field.tpe, ensureRight(field.tpe.defaultValue), 0))
      Value.StructureInstance(structName, mutable.Map.from(values))
    }
  }

  case object Void extends CYType {

    override def name: String = "void"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Void)
  }

  val rawTypes: List[CYType] = List(CYType.Integer, CYType.Real, CYType.Character, CYType.Text, CYType.Boolean)
}
package io.github.iltotore.cylang

import io.github.iltotore.cylang.ast.{Structure, Value}
import io.github.iltotore.cylang.eval.EvaluationError
import io.github.iltotore.cylang.util.*

import scala.collection.mutable

/**
 * The type of a value or expression.
 */
sealed trait CYType {

  /**
   * The string representation of this type
   */
  def name: String

  /**
   * The default value of this type (when annotating a variable)
   */
  def defaultValue(using Context): Either[EvaluationError, Value]

  /**
   * Check if this type is a subtype of the given type.
   *
   * @param other the potentially parent type
   * @return true if this type is assignable from `other`
   */
  def isSubTypeOf(other: CYType): Boolean = this.equals(other) || other.equals(CYType.Any)

  override def toString: String = name
}

object CYType {

  val rawTypes: List[CYType] = List(CYType.Any, CYType.Integer, CYType.Real, CYType.Character, CYType.Text, CYType.Boolean)

  sealed trait Number extends CYType

  case class Array(innerType: CYType, size: Option[Int]) extends CYType {

    override def name: String = s"tableau de $innerType de taille ${size.getOrElse("inconnue")}"

    override def defaultValue(using Context): Either[EvaluationError, Value] = either {
      if (size.isEmpty) left(EvaluationError("Les variables ne peuvent pas stocker un tableau sans taille définie"))
      val values = for (i <- 0 until size.get) yield ensureRight(innerType.defaultValue)
      Value.Array(values.toArray)
    }

    override def isSubTypeOf(other: CYType): Boolean = other match {

      case Array(innerType, Some(size)) => this.innerType.isSubTypeOf(innerType) && this.size.contains(size)

      case Array(innerType, None) => this.innerType.isSubTypeOf(innerType)

      case _ => super.isSubTypeOf(other)
    }
  }

  case class EnumerationField(enumName: String) extends CYType {

    override def name: String = s"ENUMERATION $enumName"

    override def defaultValue(using context: Context): Either[EvaluationError, Value] =
      for {
        enumeration <- context.scope.enumerations.get(enumName).toRight(EvaluationError(s"L'énumération '$enumName' n'existe pas"))
        field <- enumeration.fields.headOption.toRight(EvaluationError(s"L'énumération '$enumName' n'a pas de constante"))
      } yield Value.EnumerationField(enumName, field)
  }

  case class StructureInstance(structName: String) extends CYType {

    override def name: String = s"STRUCTURE $structName"

    override def defaultValue(using context: Context): Either[EvaluationError, Value] = either {
      val structure = ensureOption(context.scope.structures.get(structName))(EvaluationError(s"La structure '$structName' n'existe pas"))
      val values = for (field <- structure.fields) yield (field.name, Variable(field.tpe, ensureRight(field.tpe.defaultValue), true))
      Value.StructureInstance(structName, mutable.Map.from(values))
    }
  }

  case object Real extends Number {

    override def name: String = "reel"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Real(0))
  }

  case object Integer extends Number {

    override val name: String = "entier"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Integer(0))

    override def isSubTypeOf(other: CYType): Boolean = other.equals(CYType.Real) || super.isSubTypeOf(other)
  }

  case object Character extends CYType {

    override def name: String = "caractere"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Character(0.toChar))
  }

  case object Text extends CYType {

    override def name: String = "texte"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Text(""))
  }

  case object Boolean extends CYType {

    override def name: String = "booleen"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Bool(true))
  }

  case object Any extends CYType {

    override def name: String = "inconnu"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Left(EvaluationError("Ce type ne peut pas avoir de valeur par défaut"))

  }

  case object Void extends CYType {

    override def name: String = "void"

    override def defaultValue(using Context): Either[EvaluationError, Value] = Right(Value.Void)
  }
}

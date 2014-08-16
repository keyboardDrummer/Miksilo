package transformations.javac.methods

import core.exceptions.BadInputException
import core.transformation.{MetaObject, TransformationState}
import transformations.types.TypeC

import scala.collection.mutable

case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: MetaObject)

class VariablePool(state: TransformationState) {
  private val variables = mutable.Map[String, VariableInfo]()
  var offset = 0

  def localCount = offset

  def get(name: String) = variables.get(name)

  def apply(name: String) = variables.getOrElse(name, throw new VariableDoesNotExist(name))

  def contains(name: String) = variables.contains(name)

  def add(variable: String, _type: MetaObject) {
    variables(variable) = new VariableInfo(offset, _type)
    offset += TypeC.getTypeSize(_type, state)
  }
}

case class MethodCompiler(transformationState: TransformationState) {
  val variables = new VariablePool(transformationState)
}

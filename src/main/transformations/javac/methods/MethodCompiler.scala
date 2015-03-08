package transformations.javac.methods

import core.exceptions.BadInputException
import core.particles.{MetaObject, CompilationState}
import transformations.types.TypeSkeleton

import scala.collection.mutable

case class VariableDoesNotExist(name: String) extends BadInputException {
  override def toString = s"variable '$name' does not exist."
}

case class VariableInfo(offset: Integer, _type: MetaObject)

class VariablePool(state: CompilationState) {
  private val variables = mutable.Map[String, VariableInfo]()
  var offset = 0

  def localCount = offset

  def get(name: String) = variables.get(name)

  def apply(name: String) = variables.getOrElse(name, throw new VariableDoesNotExist(name))

  def contains(name: String) = variables.contains(name)

  def add(variable: String, _type: MetaObject) {
    variables(variable) = new VariableInfo(offset, _type)
    offset += TypeSkeleton.getTypeSize(_type, state)
  }
}

case class MethodCompiler(transformationState: CompilationState) {
  val variables = new VariablePool(transformationState)
}

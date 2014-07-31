package transformations.javac.base

import core.transformation.MetaObject

import scala.collection.mutable

case class VariableInfo(offset: Integer, _type: Any)

class VariablePool {
  private val variables = mutable.Map[String, VariableInfo]()
  var offset = 0

  def localCount = variables.size

  def apply(name: String) = variables(name)

  def add(variable: String, _type: Any) {
    variables(variable) = new VariableInfo(offset, _type)
    offset += JavaMethodC.getTypeSize(_type)
  }
}


case class MethodCompiler(classCompiler: ClassCompiler) {
  val variables = new VariablePool()

  def getReferenceKind(expression: MetaObject): ReferenceKind = {
    val getReferenceKindOption = JavaMethodC.getReferenceKindRegistry(transformationState).get(expression.clazz)
    getReferenceKindOption.fold(???)(implementation => implementation(expression))
  }

  def transformationState = classCompiler.transformationState
}

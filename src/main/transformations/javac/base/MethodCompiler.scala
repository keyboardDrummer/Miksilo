package transformations.javac.base

import core.transformation.MetaObject
import transformations.javac.methods.{CallC, SelectorC, VariableC}

import scala.collection.mutable

case class VariableInfo(offset: Integer, _type: Any)

class VariablePool {
  var offset = 0
  val variables = mutable.Map[String, VariableInfo]()

  def add(variable: String, _type: Any) {
    variables(variable) = new VariableInfo(offset, _type)
    offset += JavaMethodC.getTypeSize(_type)
  }
}

trait ReferenceKind

case class PackageReference(info: PackageInfo) extends ReferenceKind

case class ClassOrObjectReference(info: ClassInfo, wasClass: Boolean) extends ReferenceKind

case class MethodCompiler(classCompiler: ClassCompiler) {
  def transformationState = classCompiler.transformationState

  val variables = new VariablePool()
  def localCount = variables.variables.size

  def getReferenceKind(expression: MetaObject): ReferenceKind = {
    expression.clazz match {
      case SelectorC.SelectorKey =>
        val obj = SelectorC.getSelectorObject(expression)
        val member = SelectorC.getSelectorMember(expression)
        getReferenceKind(obj) match {
          case PackageReference(info) => info.content(member) match {
            case result: PackageInfo => new PackageReference(result)
            case result: ClassInfo => new ClassOrObjectReference(result, true)
          }
          case ClassOrObjectReference(info, _) =>
            val field = info.getField(member)
            val fieldClassType = classCompiler.findClass(field._type.asInstanceOf[MetaObject])
            new ClassOrObjectReference(fieldClassType, false)
        }
      case VariableC.VariableKey =>
        val name = VariableC.getVariableName(expression)
        val isClass = classCompiler.classNames.contains(name)
        if (isClass)
          new ClassOrObjectReference(classCompiler.findClass(name), true)
        else {
          val mbPackage = classCompiler.compiler.env.content.get(name)
          if (mbPackage.isDefined)
            new PackageReference(mbPackage.get.asInstanceOf[PackageInfo])
          else {
            val classInfo = classCompiler.findClass(variables.variables(name)._type.asInstanceOf[MetaObject])
            new ClassOrObjectReference(classInfo, false)
          }
        }
      case CallC.CallKey => ???

    }
  }
}

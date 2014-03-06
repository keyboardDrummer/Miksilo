package languages.javac.base

import scala.collection.mutable
import transformation.MetaObject
import languages.javac.base.JavaBaseModel._
import languages.javac.base.ClassInfo
import languages.javac.base.ClassOrObjectReference
import languages.javac.base.PackageInfo
import languages.javac.base.PackageReference
import languages.javac.base.ClassInfo
import languages.javac.base.ClassOrObjectReference
import languages.javac.base.PackageInfo
import languages.javac.base.PackageReference

case class VariableInfo(offset: Integer, _type: Any)

class VariablePool {
  var offset = 0
  val variables = mutable.Map[String, VariableInfo]()

  def add(variable: String, _type: Any) {
    variables(variable) = new VariableInfo(offset, _type)
    offset += JavaBase.getSize(_type)
  }
}

trait ReferenceKind

case class PackageReference(info: PackageInfo) extends ReferenceKind

case class ClassOrObjectReference(info: ClassInfo, wasClass: Boolean) extends ReferenceKind

case class MethodCompiler(classCompiler: ClassCompiler) {
  def transformationState = classCompiler.transformationState

  val variables = new VariablePool()
  var localCount = 0

  def getReferenceKind(expression: MetaObject): ReferenceKind = {
    expression.clazz match {
      case SelectorKey =>
        val obj = JavaBaseModel.getSelectorObject(expression)
        val member = JavaBaseModel.getSelectorMember(expression)
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
      case VariableKey =>
        val name = getVariableName(expression)
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
      case CallKey => ???

    }
  }
}

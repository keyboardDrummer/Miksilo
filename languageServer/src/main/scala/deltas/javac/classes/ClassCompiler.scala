package deltas.javac.classes

import java.util.NoSuchElementException

import core.language.node.Node
import core.language.Compilation
import deltas.bytecode.constants._
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.{QualifiedObjectTypeDelta, UnqualifiedObjectTypeDelta}
import deltas.javac.classes.skeleton.JavaClassDelta._
import deltas.javac.classes.skeleton._

case class FieldInfo(parent: ClassSignature, name: String, _static: Boolean, _type: Node) extends ClassMember

case class MethodInfo(_type: Node, _static: Boolean) extends ClassMember

case class MethodQuery(className: QualifiedClassName, methodName: String, argumentTypes: Seq[Node])

case class ClassCompiler(currentClass: Node, compilation: Compilation) {
  val javaCompiler = JavaClassDelta.state(compilation).javaCompiler
  val className: String = currentClass.name
  val myPackage: PackageSignature = javaCompiler.getPackage(currentClass._package.toList)
  val currentClassInfo = ClassSignature(myPackage, className)
  lazy val classNames: Map[String, QualifiedClassName] = getClassMapFromImports(currentClass.imports)

  def bind(): Unit = {
    val previous = JavaClassDelta.state(compilation).classCompiler
    JavaClassDelta.state(compilation).classCompiler = this
    myPackage.content(className) = currentClassInfo

    for (member <- members.get(compilation).values)
      member.bind(compilation, currentClassInfo, currentClass)
    JavaClassDelta.state(compilation).classCompiler = previous
  }

  def findClass(className: String): ClassSignature = javaCompiler.find(fullyQualify(className).parts).asInstanceOf[ClassSignature]

  def fullyQualify(className: String): QualifiedClassName = {
    try
      {
        classNames(className)
      }
    catch {
      case _:NoSuchElementException =>
        throw new NoSuchElementException(s"Could not find $className in $classNames")
    }
  }

  def getMethodRefIndex(methodKey: MethodQuery) = {
    val classRef = ClassInfoConstant.classRef(methodKey.className)
    val nameAndTypeIndex = getMethodNameAndTypeIndex(methodKey)
    MethodRefConstant.methodRef(classRef, nameAndTypeIndex)
  }

  def getMethodNameAndTypeIndex(methodKey: MethodQuery) = {
    val methodNameIndex = getNameIndex(methodKey.methodName)
    NameAndTypeConstant.nameAndType(methodNameIndex, TypeConstant.constructor(javaCompiler.find(methodKey)._type))
  }

  def getNameIndex(methodName: String) = {
    Utf8ConstantDelta.create(methodName)
  }

  def getClassRef(info: ClassSignature): Node = {
    ClassInfoConstant.classRef(info.getQualifiedName)
  }

  def findClass(objectType: Node): ClassSignature = {
    val qualifiedName = objectType.shape match {
      case QualifiedObjectTypeDelta.Shape => QualifiedObjectTypeDelta.getName(objectType)
      case UnqualifiedObjectTypeDelta.Shape => fullyQualify(UnqualifiedObjectTypeDelta.getName(objectType))
    }
    javaCompiler.find(qualifiedName.parts).asInstanceOf[ClassSignature]
  }

  private def getClassMapFromImports(imports: Seq[Node]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      JavaClassDelta.importToClassMap(compilation, _import.shape)(compilation, _import)
    }).toMap ++ Map(className -> JavaClassDelta.getQualifiedClassName(currentClass))
  }
}
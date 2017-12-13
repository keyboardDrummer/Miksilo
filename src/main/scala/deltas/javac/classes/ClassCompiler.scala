package deltas.javac.classes

import java.util.NoSuchElementException

import core.deltas.Compilation
import core.deltas.node.Node
import deltas.bytecode.constants._
import deltas.bytecode.extraConstants.TypeConstant
import deltas.bytecode.types.ObjectTypeDelta
import deltas.javac.classes.skeleton.JavaClassSkeleton._
import deltas.javac.classes.skeleton._

case class FieldInfo(parent: ClassSignature, name: String, _static: Boolean, _type: Node) extends ClassMember

case class MethodInfo(_type: Node, _static: Boolean) extends ClassMember

case class MethodQuery(className: QualifiedClassName, methodName: String, argumentTypes: Seq[Node])

case class ClassCompiler(currentClass: Node, compilation: Compilation) {
  val javaCompiler = JavaClassSkeleton.getState(compilation).javaCompiler
  val className: String = currentClass.name
  val myPackage: PackageSignature = javaCompiler.getPackage(currentClass._package.toList)
  val currentClassInfo = ClassSignature(myPackage, className)
  lazy val classNames: Map[String, QualifiedClassName] = getClassMapFromImports(currentClass.imports)

  def bind() = {
    val previous = getState(compilation).classCompiler
    getState(compilation).classCompiler = this
    myPackage.content(className) = currentClassInfo

    for (member <- getRegistry(compilation.language).members)
      member.bind(compilation, currentClassInfo, currentClass)
    getState(compilation).classCompiler = previous
  }

  def findClass(className: String): ClassSignature = javaCompiler.find(fullyQualify(className).parts).asInstanceOf[ClassSignature]

  def fullyQualify(className: String): QualifiedClassName = {
    try
      {
        classNames(className)
      }
    catch {
      case (_:NoSuchElementException) =>
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

  def getFieldRef(info: FieldInfo) = {
    val classRef = getClassRef(info.parent)
    val fieldNameAndType = getFieldNameAndType(info)
    FieldRefConstant.fieldRef(classRef, fieldNameAndType)
  }

  def getClassRef(info: ClassSignature) = {
    ClassInfoConstant.classRef(info.getQualifiedName)
  }

  def getFieldNameAndType(info: FieldInfo) = {
    val fieldNameIndex = Utf8ConstantDelta.create(info.name)
    NameAndTypeConstant.nameAndType(fieldNameIndex, TypeConstant.constructor(info._type))
  }

  def findClass(objectType: Node): ClassSignature = {
    val qualifiedName = ObjectTypeDelta.getObjectTypeName(objectType) match {
      case Right(qualified) => qualified
      case Left(name) => fullyQualify(className)
    }
    javaCompiler.find(qualifiedName.parts).asInstanceOf[ClassSignature]
  }

  private def getClassMapFromImports(imports: Seq[Node]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      JavaClassSkeleton.getRegistry(compilation.language).importToClassMap(_import.shape)(compilation, _import)
    }).toMap ++ Map(className -> JavaClassSkeleton.getQualifiedClassName(currentClass))
  }
}
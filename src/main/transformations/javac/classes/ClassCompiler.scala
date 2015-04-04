package transformations.javac.classes

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.constants.{ClassRefConstant, FieldRefConstant, MethodRefConstant, NameAndType}
import transformations.bytecode.types.ObjectTypeC
import transformations.javac.classes.skeleton._
import JavaClassSkeleton._

object ClassCompiler {
}


case class FieldInfo(parent: ClassInfo, name: String, _static: Boolean, _type: Node) extends ClassMember {
}

case class MethodInfo(descriptor: Node, _static: Boolean) extends ClassMember

case class MethodId(className: QualifiedClassName, methodName: String)

case class ClassCompiler(currentClass: Node, state: CompilationState) {
  val compiler = new MyCompiler()
  val myPackage = compiler.getPackage(currentClass._package.toList)
  val className = JavaClassSkeleton.getClassName(currentClass)
  val currentClassInfo = myPackage.newClassInfo(className)

  ByteCodeSkeleton.getState(state).constantPool = new ConstantPool()

  def constantPool = ByteCodeSkeleton.getState(state).constantPool

  lazy val classNames = getClassMapFromImports(currentClass.imports)

  def findClass(className: String) = compiler.find(fullyQualify(className).parts).asInstanceOf[ClassInfo]

  def fullyQualify(className: String): QualifiedClassName = classNames(className)

  def findMethod(methodRef: Node): MethodInfo = {
    val classIndex = MethodRefConstant.getMethodRefClassRefIndex(methodRef)
    val classRef = constantPool.getValue(classIndex).asInstanceOf[Node]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[QualifiedClassName]
    val nameAndTypeIndex = MethodRefConstant.getMethodRefMethodNameIndex(methodRef)
    val nameAndType = constantPool.getValue(nameAndTypeIndex).asInstanceOf[Node]
    val methodName = constantPool.getValue(NameAndType.getNameAndTypeName(nameAndType)).asInstanceOf[String]
    val methodId = new MethodId(className, methodName)
    compiler.find(methodId)
  }

  def getMethodRefIndex(methodKey: MethodId): Int = {
    val classRefIndex = constantPool.getClassRef(methodKey.className)
    val nameAndTypeIndex = getMethodNameAndTypeIndex(methodKey)
    constantPool.store(MethodRefConstant.methodRef(classRefIndex, nameAndTypeIndex))
  }

  def getMethodNameAndTypeIndex(methodKey: MethodId): Int = {
    val methodNameIndex = getNameIndex(methodKey.methodName)
    val descriptorIndex = constantPool.store(compiler.find(methodKey).descriptor)
    val result: Node = NameAndType.nameAndType(methodNameIndex, descriptorIndex)
    constantPool.store(result)
  }

  def getNameIndex(methodName: String): Int = {
    constantPool.storeUtf8(methodName)
  }

  def getFieldRefIndex(info: FieldInfo): Int = {
    val classRef = getClassRef(info.parent)
    val fieldNameAndTypeIndex = getFieldNameAndTypeIndex(info)
    constantPool.store(FieldRefConstant.fieldRef(classRef, fieldNameAndTypeIndex))
  }

  def getClassRef(info: ClassInfo): Int = {
    constantPool.getClassRef(info.getQualifiedName)
  }

  def getFieldNameAndTypeIndex(info: FieldInfo): Int = {
    val fieldNameIndex = constantPool.storeUtf8(info.name)
    val typeIndex =   constantPool.store(info._type)
    val result: Node = NameAndType.nameAndType(fieldNameIndex, typeIndex)
    constantPool.store(result)
  }

  def findClass(objectType: Node): ClassInfo = {
    val qualifiedName = ObjectTypeC.getObjectTypeName(objectType) match {
      case Right(qualified) => qualified
      case Left(name) => fullyQualify(className)
    }
    compiler.find(qualifiedName.parts).asInstanceOf[ClassInfo]
  }

  private def getClassMapFromImports(imports: Seq[Node]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      JavaClassSkeleton.getState(state).importToClassMap(_import.clazz)(_import)
    }).toMap ++ Map(className -> JavaClassSkeleton.getQualifiedClassName(currentClass))
  }
}
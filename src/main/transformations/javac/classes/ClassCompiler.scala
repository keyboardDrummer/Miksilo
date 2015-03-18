package transformations.javac.classes

import core.particles.CompilationState
import core.particles.node.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.constants.{ClassRefConstant, FieldRefConstant, MethodRefConstant, NameAndType}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.types.ObjectTypeC

object ClassCompiler {
}


case class FieldInfo(parent: ClassInfo, name: String, _static: Boolean, _type: MetaObject) extends ClassMember {
}

case class MethodInfo(descriptor: MetaObject, _static: Boolean) extends ClassMember

case class MethodId(className: QualifiedClassName, methodName: String)

case class ClassCompiler(currentClass: MetaObject, state: CompilationState) {
  val compiler = new MyCompiler()
  val myPackage = compiler.getPackage(JavaClassSkeleton.getPackage(currentClass).toList)
  val className = JavaClassSkeleton.getClassName(currentClass)
  val currentClassInfo = myPackage.newClassInfo(className)

  ByteCodeSkeleton.getState(state).constantPool = new ConstantPool()

  def constantPool = ByteCodeSkeleton.getState(state).constantPool

  lazy val classNames = getClassMapFromImports(JavaClassSkeleton.getImports(currentClass))

  def findClass(className: String) = compiler.find(fullyQualify(className).parts).asInstanceOf[ClassInfo]

  def fullyQualify(className: String): QualifiedClassName = classNames(className)

  def findMethod(methodRef: MetaObject): MethodInfo = {
    val classIndex = MethodRefConstant.getMethodRefClassRefIndex(methodRef)
    val classRef = constantPool.getValue(classIndex).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[QualifiedClassName]
    val nameAndTypeIndex = MethodRefConstant.getMethodRefMethodNameIndex(methodRef)
    val nameAndType = constantPool.getValue(nameAndTypeIndex).asInstanceOf[MetaObject]
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
    val result: MetaObject = NameAndType.nameAndType(methodNameIndex, descriptorIndex)
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
    val result: MetaObject = NameAndType.nameAndType(fieldNameIndex, typeIndex)
    constantPool.store(result)
  }

  def findClass(objectType: MetaObject): ClassInfo = {
    val qualifiedName = ObjectTypeC.getObjectTypeName(objectType) match {
      case Right(qualified) => qualified
      case Left(name) => fullyQualify(className)
    }
    compiler.find(qualifiedName.parts).asInstanceOf[ClassInfo]
  }

  private def getClassMapFromImports(imports: Seq[MetaObject]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      JavaClassSkeleton.getState(state).importToClassMap(_import.clazz)(_import)
    }).toMap ++ Map(className -> JavaClassSkeleton.getQualifiedClassName(currentClass))
  }
}
package transformations.javac.classes

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.expressions.ExpressionC
import transformations.types.ObjectTypeC

object ClassCompiler {
}


case class FieldInfo(parent: ClassInfo, name: String, _type: MetaObject) extends ClassMember {
}

case class MethodInfo(descriptor: MetaObject, _static: Boolean) extends ClassMember

case class MethodId(className: QualifiedClassName, methodName: String)

case class ClassCompiler(currentClass: MetaObject, state: TransformationState) {


  val compiler = new MyCompiler()
  val myPackage = compiler.getPackage(ClassC.getPackage(currentClass).toList)
  val className = ClassC.getClassName(currentClass)
  val currentClassInfo = myPackage.newClassInfo(className)

  ByteCodeSkeleton.getState(state).constantPool = new ConstantPool()
  def constantPool = ByteCodeSkeleton.getState(state).constantPool

  val classNames = getClassMapFromImports(ClassC.getImports(currentClass))

  def findClass(className: String) = compiler.find(fullyQualify(className).parts).asInstanceOf[ClassInfo]

  def fullyQualify(className: String): QualifiedClassName = classNames(className)

  def findMethod(methodRef: MetaObject): MethodInfo = {
    val classIndex = ByteCodeSkeleton.getMethodRefClassRefIndex(methodRef)
    val classRef = constantPool.getValue(classIndex).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCodeSkeleton.getClassRefName(classRef)).asInstanceOf[QualifiedClassName]
    val nameAndTypeIndex = ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)
    val nameAndType = constantPool.getValue(nameAndTypeIndex).asInstanceOf[MetaObject]
    val methodName = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeName(nameAndType)).asInstanceOf[String]
    val methodId = new MethodId(className, methodName)
    compiler.find(methodId)
  }

  def getMethodRefIndex(methodKey: MethodId): Int = {
    val classRefIndex = constantPool.getClassRef(methodKey.className)
    val nameAndTypeIndex = getMethodNameAndTypeIndex(methodKey)
    constantPool.store(ByteCodeSkeleton.methodRef(classRefIndex, nameAndTypeIndex))
  }

  def getMethodNameAndTypeIndex(methodKey: MethodId): Int = {
    val methodNameIndex = getMethodNameIndex(methodKey.methodName)
    val descriptorIndex = constantPool.store(compiler.find(methodKey).descriptor)
    val result: MetaObject = ByteCodeSkeleton.nameAndType(methodNameIndex, descriptorIndex)
    constantPool.store(result)
  }

  def getMethodNameIndex(methodName: String): Int = {
    constantPool.storeUtf8(methodName)
  }

  def getFieldRefIndex(info: FieldInfo): Int = {
    val classRef = getClassRef(info.parent)
    val fieldNameAndTypeIndex = getFieldNameAndTypeIndex(info)
    constantPool.store(ByteCodeSkeleton.fieldRef(classRef, fieldNameAndTypeIndex))
  }

  def getClassRef(info: ClassInfo): Int = {
    constantPool.getClassRef(info.getQualifiedName)
  }

  def getFieldNameAndTypeIndex(info: FieldInfo): Int = {
    val fieldNameIndex = constantPool.storeUtf8(info.name)
    val typeIndex = constantPool.store(info._type)
    val result: MetaObject = ByteCodeSkeleton.nameAndType(fieldNameIndex, typeIndex)
    constantPool.store(result)
  }

  def getReferenceKind(expression: MetaObject): ReferenceKind = {
    val getReferenceKindOption = ClassC.getReferenceKindRegistry(state).get(expression.clazz)
    getReferenceKindOption.fold[ReferenceKind]({
      getReferenceKindFromExpressionType(expression)
    })(implementation => implementation(expression))
  }

  def getReferenceKindFromExpressionType(expression: MetaObject): ClassOrObjectReference = {
    val classInfo: ClassInfo = findClass(ExpressionC.getType(state)(expression))
    new ClassOrObjectReference(classInfo, false)
  }

  def findClass(objectType: MetaObject): ClassInfo = {
    val qualifiedName = ObjectTypeC.getObjectTypeName(objectType) match {
      case Right(qualified) => qualified
      case Left(name) => fullyQualify(className)
    }
    compiler.find(qualifiedName.parts).asInstanceOf[ClassInfo]
  }

  private def getClassMapFromImports(imports: Seq[JavaImport]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      val finalPackage = compiler.find(_import._package).asInstanceOf[PackageInfo]
      val result: Iterable[(String, QualifiedClassName)] = _import.end match {
        case Some(importedClassName) =>
          val qualifiedClassName = new QualifiedClassName(_import._package ++ Seq(importedClassName))
          Seq((importedClassName, qualifiedClassName)).toMap
        case _ => finalPackage.flattenContents().map(entry =>
          (entry._1.last, new QualifiedClassName(_import._package ++ entry._1)))
      }
      result
    }).toMap ++ Map(className -> ClassC.getQualifiedClassName(currentClass))
  }
}
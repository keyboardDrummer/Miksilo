package transformations.javac.base

import transformations.bytecode.ByteCode
import core.transformation.{TransformationState, MetaObject}
import scala.Some
import transformations.javac.base.model.{JavaTypes, JavaClassModel, JavaImport, QualifiedClassName}

object ClassCompiler {
}


case class FieldInfo(parent: ClassInfo, name: String, _type: Any) extends ClassMember {
}

case class MethodInfo(descriptor: MetaObject, _static: Boolean) extends ClassMember

case class MethodId(className: QualifiedClassName, methodName: String)

case class ClassCompiler(currentClass: MetaObject, transformationState: TransformationState) {


  val compiler = new MyCompiler()
  val myPackage = compiler.getPackage(JavaClassModel.getPackage(currentClass).toList)
  val className = JavaClassModel.getClassName(currentClass)
  val currentClassInfo = myPackage.newClassInfo(className)

  val constantPool = new ConstantPool()

  def fullyQualify(className: String): QualifiedClassName = classNames(className)

  def findClass(objectType: MetaObject) = {
    val qualifiedName = JavaTypes.getObjectTypeName(objectType) match {
      case Right(qualified) => qualified
      case Left(name) => fullyQualify(className)
    }
    compiler.find(qualifiedName.parts).asInstanceOf[ClassInfo]
  }

  def findClass(className: String) = compiler.find(fullyQualify(className).parts).asInstanceOf[ClassInfo]

  val classNames = getClassMapFromImports(JavaClassModel.getImports(currentClass))

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
    }).toMap ++ Map(className -> JavaBase.getQualifiedClassName(currentClass))
  }

  def getClassRef(info: ClassInfo): Int = {
    getClassRef(info.getQualifiedName)
  }

  def getClassRef(nameParts: QualifiedClassName): Int = {
    val nameIndex = constantPool.store(nameParts)
    constantPool.store(ByteCode.classRef(nameIndex))
  }


  def findMethod(methodRef: MetaObject): MethodInfo = {
    val classIndex = ByteCode.getMethodRefClassRefIndex(methodRef)
    val classRef = constantPool.getValue(classIndex).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCode.getClassRefName(classRef)).asInstanceOf[QualifiedClassName]
    val nameAndTypeIndex = ByteCode.getMethodRefMethodNameIndex(methodRef)
    val nameAndType = constantPool.getValue(nameAndTypeIndex).asInstanceOf[MetaObject]
    val methodName = constantPool.getValue(ByteCode.getNameAndTypeName(nameAndType)).asInstanceOf[String]
    val methodId = new MethodId(className, methodName)
    compiler.find(methodId)
  }

  def getMethodNameAndTypeIndex(methodKey: MethodId): Int = {
    val methodNameIndex = getMethodNameIndex(methodKey.methodName)
    val descriptorIndex = constantPool.store(compiler.find(methodKey).descriptor)
    val result: MetaObject = ByteCode.nameAndType(methodNameIndex, descriptorIndex)
    constantPool.store(result)
  }

  def getFieldNameAndTypeIndex(info: FieldInfo): Int = {
    val fieldNameIndex = constantPool.storeUtf8(info.name)
    val typeIndex = constantPool.store(info._type)
    val result: MetaObject = ByteCode.nameAndType(fieldNameIndex, typeIndex)
    constantPool.store(result)
  }

  def getMethodRefIndex(methodKey: MethodId): Int = {
    val classRefIndex = getClassRef(methodKey.className)
    val nameAndTypeIndex = getMethodNameAndTypeIndex(methodKey)
    constantPool.store(ByteCode.methodRef(classRefIndex, nameAndTypeIndex))
  }

  def getMethodNameIndex(methodName: String): Int = {
    constantPool.storeUtf8(methodName)
  }

  def getFieldRefIndex(info: FieldInfo): Int = {
    val classRef = getClassRef(info.parent)
    val fieldNameAndTypeIndex = getFieldNameAndTypeIndex(info)
    constantPool.store(ByteCode.fieldRef(classRef, fieldNameAndTypeIndex))
  }
}
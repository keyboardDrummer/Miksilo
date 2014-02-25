package languages.javac.base

import languages.bytecode.ByteCode
import languages.javac.base._
import languages.javac.JavaLang
import scala.collection.mutable
import transformation.{TransformationState, MetaObject}
import languages.javac.base.QualifiedClassName
import languages.javac.base.JavaImport
import languages.javac.base.MethodKey
import scala.tools.scalap.scalax.util.StringUtil

object ClassCompiler {
}


case class MethodInfo(descriptor: MetaObject)
case class MethodKey(className: QualifiedClassName, methodName: String)
case class ClassCompiler(currentClass : MetaObject, transformationState: TransformationState) {

  val compiler  = new MyCompiler()
  val constantPool = new ConstantPool()
  def fullyQualify(className: String) : QualifiedClassName = classNames(className)


  val classNames = getClassMapFromImports(JavaClassModel.getImports(currentClass))
  private def getClassMapFromImports(imports: Seq[JavaImport]): Map[String, QualifiedClassName] = {
    imports.flatMap(_import => {
      val finalPackage = compiler.find(_import._package).asInstanceOf[MyPackage]
      val result: Iterable[(String, QualifiedClassName)] = _import.end match {
        case Some(className) =>
          val qualifiedClassName = new QualifiedClassName(_import._package ++ Seq(className))
          Seq((className, qualifiedClassName)).toMap
        case _ => finalPackage.flattenContents().map(entry =>
          (entry._1.last, new QualifiedClassName(_import._package ++ entry._1)) )
      }
      result
    }).toMap
  }

  def getClassRef(nameParts: QualifiedClassName) = {
    val nameIndex = constantPool.store(nameParts)
    constantPool.store(ByteCode.classRef(nameIndex))
  }

  def getMethodNameAndType(methodKey: MethodKey): Int = {
    val methodNameIndex = getMethodNameIndex(methodKey.methodName)
    val descriptorIndex = constantPool.store(compiler.find(methodKey).descriptor)
    val result: MetaObject = ByteCode.nameAndType(methodNameIndex, descriptorIndex)
    constantPool.store(result)
  }

  def getMethodRefIndex(methodKey: MethodKey): Int = {
    val classRefIndex = getClassRef(methodKey.className)
    val nameAndTypeIndex = getMethodNameAndType(methodKey)
    constantPool.store(ByteCode.methodRef(classRefIndex, nameAndTypeIndex))
  }

  def getMethodNameIndex(methodName: String): Int = {
    constantPool.storeUtf8(methodName)
  }
}
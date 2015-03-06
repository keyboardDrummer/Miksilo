package transformations.javac.classes

import core.transformation.MetaObject
import transformations.javac.JavaLang

import scala.collection.mutable

class PackageContent(parent: Option[PackageInfo], name: String)

case class PackageInfo(parent: Option[PackageInfo], name: String, content: mutable.Map[String, PackageContent] = mutable.Map())
  extends PackageContent(parent, name) {
  def getPackage(parts: List[String]): PackageInfo = parts match {
    case Nil => this
    case ::(head, tail) => content.getOrElseUpdate(head, new PackageInfo(Some(this), head)).asInstanceOf[PackageInfo].getPackage(tail)
  }

  def getQualifiedName: QualifiedClassName = new QualifiedClassName(parent.fold(Seq[String]())(info => info.getQualifiedName.parts ++ Seq(name)))

  def flattenContents(): mutable.Map[List[String], ClassInfo] = {
    content.flatMap(item => item._2 match {
      case classInfo: ClassInfo => Map(List(item._1) -> classInfo)
      case newPackage: PackageInfo => newPackage.flattenContents().map(
        entry => (item._1 :: entry._1, entry._2))
    })
  }

  def newClassInfo(className: String) = {
    val result = new ClassInfo(this, className)
    content(className) = result
    result
  }

  def newPackageInfo(packageName: String) = {
    val result = new PackageInfo(Some(this), packageName)
    content(packageName) = result
    result
  }
}

trait ClassMember
{
  def _static: Boolean
}

case class ClassInfo(parent: PackageInfo, name: String, content: mutable.Map[String, ClassMember] = mutable.Map()) extends PackageContent(Some(parent), name) {
  def getMethod(name: String) = content(name).asInstanceOf[MethodInfo]

  def getField(name: String) = content(name).asInstanceOf[FieldInfo]

  def getQualifiedName: QualifiedClassName = new QualifiedClassName(parent.getQualifiedName.parts ++ Seq(name))

  def newFieldInfo(name: String, _type: MetaObject, _static: Boolean = false) = {
    val result = new FieldInfo(this, name, _static, _type)
    content(name) = result
    result
  }

  def newMethodInfo(name: String, descriptor: MetaObject, _static: Boolean) = {
    val result = new MethodInfo(descriptor, _static)
    content(name) = result
    result
  }
}

class MyCompiler {
  val env: PackageInfo = JavaLang.standardLib

  def getPackage(parts: List[String]): PackageInfo = env.getPackage(parts)

  def find(methodId: MethodId): MethodInfo = find(methodId.className.parts)
    .asInstanceOf[ClassInfo].getMethod(methodId.methodName)

  def find(parts: Seq[String]): PackageContent = parts.foldLeft[PackageContent](env)(
    (pck: PackageContent, part: String) => pck.asInstanceOf[PackageInfo].content(part))
}
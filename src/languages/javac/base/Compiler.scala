package languages.javac.base

import scala.collection.mutable
import languages.javac.JavaLang
import languages.javac.base.MethodKey

trait PackageContent
case class MyPackage(content: mutable.Map[String, PackageContent] = mutable.Map()) extends PackageContent
{
  def getPackage(parts: List[String]) : MyPackage = parts match {
    case Nil => this
    case ::(head,tail) => content.getOrElse(head, new MyPackage()).asInstanceOf[MyPackage].getPackage(tail)
  }
  def flattenContents() : mutable.Map[List[String],ClassInfo] = {
    content.flatMap(item => item._2 match {
      case classInfo: ClassInfo => Map(List(item._1) -> classInfo)
      case newPackage: MyPackage => newPackage.flattenContents().map(
        entry => (item._1 :: entry._1, entry._2))
    })
  }
}
case class ClassInfo(content: mutable.Map[String, MethodInfo] = mutable.Map()) extends PackageContent

class MyCompiler {
  val env: MyPackage = JavaLang.standardLib
  def getPackage(parts: List[String]) : MyPackage = env.getPackage(parts)
  def find(parts: Seq[String]) : PackageContent = parts.foldLeft[PackageContent](env)(
    (pck: PackageContent,part: String) => pck.asInstanceOf[MyPackage].content(part))

  def find(methodKey: MethodKey) : MethodInfo = find(methodKey.className.parts)
    .asInstanceOf[ClassInfo].content(methodKey.methodName)
}
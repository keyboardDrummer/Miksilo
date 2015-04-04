package transformations.javac.classes.skeleton

import scala.collection.mutable
import core.particles.node.Node
import JavaClassSkeleton._

class PackageMember(parent: Option[PackageInfo], name: String)
case class PackageInfo(parent: Option[PackageInfo], name: String, content: mutable.Map[String, PackageMember] = mutable.Map())
  extends PackageMember(parent, name) {
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

     def addClass(clazz: Node) = {
       val name = clazz.name
       val classInfo = newClassInfo(name)
     }

     def newClassInfo(className: String) = {
       val result = new ClassInfo(this, className)
       content(className) = result
       result
     }

     def findOrCreate(packageName: String): PackageInfo = {
       content.getOrElseUpdate(packageName, new PackageInfo(Some(this), packageName)).asInstanceOf[PackageInfo]
     }

     def newPackageInfo(packageName: String) = {
       val result = new PackageInfo(Some(this), packageName)
       content(packageName) = result
       result
     }
   }

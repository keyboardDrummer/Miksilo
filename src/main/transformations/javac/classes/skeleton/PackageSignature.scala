package transformations.javac.classes.skeleton

import core.particles.CompilationState
import core.particles.node.Node
import JavaClassSkeleton._
import scala.collection.mutable

class PackageMember(parent: Option[PackageSignature], name: String)
class PackageSignature(val parent: Option[PackageSignature], val name: String, val content: mutable.Map[String, PackageMember] = mutable.Map())
  extends PackageMember(parent, name) {
  def getPackage(parts: List[String]): PackageSignature = parts match {
   case Nil => this
   case ::(packageName, rest) => findOrCreate(packageName).getPackage(rest)
  }

  def getQualifiedName: QualifiedClassName = new QualifiedClassName(parent.fold(Seq[String]())(info => info.getQualifiedName.parts ++ Seq(name)))

  def flattenContents(): mutable.Map[List[String], ClassSignature] = {
   content.flatMap(item => item._2 match {
     case classInfo: ClassSignature => Map(List(item._1) -> classInfo)
     case newPackage: PackageSignature => newPackage.flattenContents().map(
       entry => (item._1 :: entry._1, entry._2))
   })
  }

  def addClass(state: CompilationState, clazz: Node) {
   for (firstMemberPass <- getState(state).firstMemberPasses)
     firstMemberPass(clazz)
  }

  def findPackageSignature(clazz: Node): PackageSignature = {
   clazz._package.foldLeft(this)((currentPackage, packageName) => currentPackage.findOrCreate(packageName))
  }

  def findOrCreate(packageName: String): PackageSignature = {
   content.getOrElseUpdate(packageName, new PackageSignature(Some(this), packageName)).asInstanceOf[PackageSignature]
  }

  def newPackageInfo(packageName: String) = {
   val result = new PackageSignature(Some(this), packageName)
   content(packageName) = result
   result
  }
}

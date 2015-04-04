package transformations.javac.classes.skeleton

import core.particles.node.Node
import transformations.javac.classes.{FieldInfo, MethodInfo}

import scala.collection.mutable

case class ClassInfo(parent: PackageInfo, name: String, content: mutable.Map[String, ClassMember] = mutable.Map()) extends PackageMember(Some(parent), name) {
  def getMethod(name: String) = content(name).asInstanceOf[MethodInfo]

  def getField(name: String) = content(name).asInstanceOf[FieldInfo]

  def getQualifiedName: QualifiedClassName = new QualifiedClassName(parent.getQualifiedName.parts ++ Seq(name))

  def newFieldInfo(name: String, _type: Node, _static: Boolean = false) = {
    val result = new FieldInfo(this, name, _static, _type)
    content(name) = result
    result
  }

  def newMethodInfo(name: String, descriptor: Node, _static: Boolean) = {
    val result = new MethodInfo(descriptor, _static)
    content(name) = result
    result
  }
}

trait ClassMember {
   def _static: Boolean
 }

package transformations.javac.classes.skeleton

import core.particles.node.Node
import transformations.javac.classes.{MethodQuery, FieldInfo, MethodInfo}
import transformations.javac.types.MethodTypeC._

import scala.collection.mutable

case class MethodClassKey(methodName: String, parameterTypes: Vector[Node])

case class ClassSignature(parent: PackageSignature, name: String, 
                          methods: mutable.Map[MethodClassKey, MethodInfo] = mutable.Map(),
                          fields: mutable.Map[String, FieldInfo] = mutable.Map())
  extends PackageMember(Some(parent), name) {
  
  def getMethod(query: MethodQuery): MethodInfo = {
    val key: MethodClassKey = new MethodClassKey(query.methodName, query.argumentTypes.toVector)
    try
    {
      methods(key)
    } catch
    {
      case e: NoSuchElementException => throw new NoSuchElementException(s"couldn't find $key in map ${methods.toString}") //TODO remove?
    }
  }

  def getField(name: String) = fields(name)

  def getQualifiedName: QualifiedClassName = new QualifiedClassName(parent.getQualifiedName.parts ++ Seq(name))

  def newFieldInfo(name: String, _type: Node, _static: Boolean = false) = {
    val result = new FieldInfo(this, name, _static, _type)
    fields(name) = result
    result
  }

  def newMethodInfo(name: String, methodType: Node, _static: Boolean) = {
    val result = new MethodInfo(methodType, _static)
    methods(new MethodClassKey(name, methodType.parameterTypes.toVector)) = result
    result
  }
}

trait ClassMember {
  def _static: Boolean
}

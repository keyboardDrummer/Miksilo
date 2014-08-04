package transformations.javac.base.model

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton

object JavaMethodModel {


  def getMethodBody(metaObject: MetaObject) = metaObject(MethodBodyKey).asInstanceOf[Seq[MetaObject]]

  def method(name: String, _returnType: Any, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
             static: Boolean = false, visibility: Visibility = PrivateVisibility) = {
    new MetaObject(ByteCodeSkeleton.MethodInfoKey) {
      data.put(MethodNameKey, name)
      data.put(ReturnTypeKey, _returnType)
      data.put(MethodParametersKey, _parameters)
      data.put(MethodBodyKey, _body)
      data.put(StaticKey, static)
      data.put(VisibilityKey, visibility)
    }
  }

  def getMethodStatic(method: MetaObject) = method(StaticKey).asInstanceOf[Boolean]

  def getMethodVisibility(method: MetaObject) = method(VisibilityKey).asInstanceOf[Visibility]

  def parameter(name: String, _type: Any) = {
    new MetaObject("JavaParameter") {
      data.put(ParameterNameKey, name)
      data.put(ParameterTypeKey, _type)
    }
  }

  def getMethodName(method: MetaObject) = {
    method(MethodNameKey).asInstanceOf[String]
  }

  def getMethodParameters(metaObject: MetaObject) = {
    metaObject(MethodParametersKey).asInstanceOf[Seq[MetaObject]]
  }

  def getMethodReturnType(metaObject: MetaObject) = {
    metaObject(ReturnTypeKey).asInstanceOf[MetaObject]
  }

  def getParameterType(metaObject: MetaObject) = metaObject(ParameterTypeKey).asInstanceOf[MetaObject]

  def getParameterName(metaObject: MetaObject) = metaObject(ParameterNameKey).asInstanceOf[String]

  class Visibility

  object MethodBodyKey

  object ParameterNameKey

  object StaticKey

  object VisibilityKey

  object PublicVisibility extends Visibility

  object ProtectedVisibility extends Visibility

  object PrivateVisibility extends Visibility

  object DefaultVisibility extends Visibility

  object ReturnTypeKey

  object MethodNameKey

  object MethodParametersKey

  object ParameterTypeKey

}

package languages.javac.base.model

import transformation.MetaObject
import languages.bytecode.ByteCode

object JavaMethodModel {

  object Return
  object ReturnValue
  def _return(value: Option[MetaObject] = None): MetaObject = new MetaObject(Return) {
    data.put(ReturnValue, value)
  }
  def getReturnValue(_return: MetaObject) = _return(ReturnValue).asInstanceOf[Option[MetaObject]]

  def getMethodBody(metaObject: MetaObject) = metaObject(MethodBodyKey).asInstanceOf[Seq[MetaObject]]

  object MethodBodyKey

  def method(name: String, _returnType: Any, _parameters: Seq[MetaObject], _body: Seq[MetaObject],
             static: Boolean = false, visibility: Visibility = PrivateVisibility) = {
    new MetaObject(ByteCode.MethodInfoKey) {
      data.put(MethodNameKey, name)
      data.put(ReturnTypeKey, _returnType)
      data.put(MethodParametersKey, _parameters)
      data.put(MethodBodyKey,_body)
      data.put(StaticKey, static)
      data.put(VisibilityKey, visibility)
    }
  }
  def getMethodStatic(method: MetaObject) = method(StaticKey).asInstanceOf[Boolean]
  def getMethodVisibility(method: MetaObject) = method(VisibilityKey).asInstanceOf[Visibility]

  object ParameterNameKey

  object StaticKey
  object VisibilityKey
  class Visibility
  object PublicVisibility extends Visibility
  object ProtectedVisibility extends Visibility
  object PrivateVisibility extends Visibility
  object DefaultVisibility extends Visibility
  def parameter(name: String, _type: Any) = {
    new MetaObject("JavaParameter") {
      data.put(ParameterNameKey, name)
      data.put(ParameterTypeKey, _type)
    }
  }

  object ReturnTypeKey
  object MethodNameKey

  def getMethodName(method: MetaObject) = {
    method(MethodNameKey).asInstanceOf[String]
  }

  object MethodParametersKey
  def getMethodParameters(metaObject: MetaObject) = {
    metaObject(MethodParametersKey).asInstanceOf[Seq[MetaObject]]
  }

  def getMethodReturnType(metaObject: MetaObject) = {
    metaObject(ReturnTypeKey)
  }

  def getParameterType(metaObject: MetaObject) : Any = metaObject(ParameterTypeKey)
  def getParameterName(metaObject: MetaObject) = metaObject(ParameterNameKey).asInstanceOf[String]
  object ParameterTypeKey

}

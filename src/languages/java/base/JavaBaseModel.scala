package languages.java.base

import transformation.MetaObject
import languages.bytecode.ByteCode
import scala.collection.mutable

object JavaClassModel {

  def clazz(name: String, methods: Seq[MetaObject]) = new MetaObject(ByteCode.ClassFileKey) {
    data.put(ByteCode.ClassMethodsKey, mutable.Buffer(methods))
    data.put(ByteCode.ClassNameKey, name)
  }
  def getClassName(clazz: MetaObject) = clazz(ByteCode.ClassNameKey).asInstanceOf[String]
  def getMethods(clazz: MetaObject) = clazz(ByteCode.ClassMethodsKey).asInstanceOf[mutable.Buffer[MetaObject]]
}

object JavaMethodModel {

  object Return
  object ReturnValue
  def _return(value: MetaObject): MetaObject = new MetaObject(Return) {
    data.put(ReturnValue, value)
  }
  def getReturnValue(_return: MetaObject) = _return(ReturnValue).asInstanceOf[MetaObject]

  def getMethodBody(metaObject: MetaObject) = metaObject(MethodBodyKey).asInstanceOf[Seq[MetaObject]]

  object Constructor
  object MethodBodyKey
  def constructor(_parameters: Seq[MetaObject], _body: Seq[MetaObject], visibility: Visibility = PublicVisibility) = new MetaObject(Constructor) {
    data.put(MethodParametersKey, _parameters)
    data.put(MethodBodyKey,_body)
    data.put(VisibilityKey, visibility)
  }

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

  object ParameterNameKey

  object StaticKey
  object VisibilityKey
  class Visibility
  object PublicVisibility extends Visibility
  object PrivateVisibility extends Visibility
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

object JavaBaseModel {

  object CallKey
  object CallCallee
  object CallArguments
  def call(callee: MetaObject, arguments: Seq[MetaObject] = Seq()) = {
    new MetaObject(CallKey) {
      data.put(CallCallee, callee)
      data.put(CallArguments, arguments)
    }
  }

  def getCallCallee(call: MetaObject) = call(CallCallee).asInstanceOf[MetaObject]
  def getCallArguments(call: MetaObject) = call(CallArguments).asInstanceOf[Seq[MetaObject]]

  object VariableKey
  val variableNameKey = "name"
  def variable(name: String) = {
    new MetaObject(VariableKey) {
      data.put(variableNameKey, name)
    }
  }

  def getVariableName(variable: MetaObject) = variable(variableNameKey).asInstanceOf[String]


  object SelectorKey
  object SelectorObject
  object SelectorMember
  def selector(selectee: MetaObject, member: String) {
    new MetaObject(SelectorKey) {
      data.put(SelectorObject, selectee)
      data.put(SelectorMember, member)
    }
  }
}

object JavaTypes {
  def objectType(className: String): Any = new MetaObject(ObjectType) {
    data.put(ObjectTypeName,className)
  }

  object ObjectTypeName
  object ObjectType
  object StringType
  object VoidType
  object IntegerType
  object DoubleType
  def arrayType(elementType: Any) = {
    new MetaObject("arrayType") { data.put("elementType", elementType) }
  }

}

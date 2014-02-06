package javaInterpreter

import transformation.MetaObject

object MetaObjectConstructors {
  def method(name: String, returnType: Any, parameters: Seq[MetaObject], body: Seq[MetaObject]) = {
    new MetaObject("JavaMethod") {
      data.put("name", name)
      data.put("returnType", returnType)
      data.put("parameters", parameters)
      data.put("body",body)
    }
  }
  def literal(value: AnyVal) = {
    new MetaObject("Literal") {
      data.put("value", value)
    }
  }
  def variable(name: String) = {
    new MetaObject("Variable") {
      data.put("name", name)
    }
  }
  def parameter(name: String, _type: Any) = {
    new MetaObject("JavaParameter") {
      data.put("name", name)
      data.put("_type", _type)
    }
  }
  def call(callee: MetaObject, arguments: Seq[MetaObject]) = {
    new MetaObject("Call") {
      data.put("callee", callee)
      data.put("arguments", arguments)
    }
  }
  def subtraction(first: MetaObject, second: MetaObject) = {
    new MetaObject("JavaSubtraction") {
      data.put("first", first)
      data.put("second", second)
    }
  }
}

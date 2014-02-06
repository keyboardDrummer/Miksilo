package javaInterpreter

object JavaType {
  def IntType = new JavaType(classOf[Integer])
}
case class JavaType(_type: Class[_]) {

}
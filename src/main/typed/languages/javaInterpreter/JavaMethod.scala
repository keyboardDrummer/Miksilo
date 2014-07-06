package typed.languages.javaInterpreter

case class JavaMethod(name: String, returnType: JavaType, parameters: Seq[JavaParameter], body: Seq[JavaStatement]) {

}

case class JavaParameter(name: String, _type: JavaType)

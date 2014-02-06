package javaInterpreter

object JavaExamples {
  def fibonacciProgram() = {
    val parameters = Seq(new JavaParameter("i", JavaType.IntType))
    val recursiveCall1 = new Call(new Variable("fibonacci"), Seq(new JavaSubtraction(new Variable("i"), new Literal(1))))
    val recursiveCall2 = new Call(new Variable("fibonacci"), Seq(new JavaSubtraction(new Variable("i"), new Literal(2))))
    val condition = new LessThan(new Variable("i"), new Literal(2))
    val body = Seq(new Ternary(condition, new Literal(1), new Addition(recursiveCall2, recursiveCall1)))
    val fibonacciMethod = new JavaMethod("fibonacci", JavaType.IntType, parameters, body)
    val methods = Seq(fibonacciMethod)
    val program = new JavaClass(methods)
    program
  }

}

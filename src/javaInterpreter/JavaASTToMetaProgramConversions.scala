package javaInterpreter

import transformation.MetaObject
import util.TypedToMetaProgramConverter
import org.junit.{Assert, Test}
import MetaObjectConstructors._

object JavaASTToMetaProgramConversions extends TypedToMetaProgramConverter {
  def toMetaProgram(java: JavaClass) : MetaObject = convert(java)

  override def convertAny(value: Any): Any = {
    value match {
      case javaType : JavaType => javaType._type.getSimpleName
      case _ => super.convertAny(value)
    }
  }
}

class JavaASTToMetaProgramConversionsTest {
  @Test
  def testFibonacciConversion() {
    val fibonacciTyped = {
      val parameters = Seq(new JavaParameter("i", JavaType.IntType))
      val recursiveCall1 = new Call(new Variable("fibonacci"), Seq(new JavaSubtraction(new Variable("i"), new Literal(1))))
      val recursiveCall2 = new Call(new Variable("fibonacci"), Seq(new JavaSubtraction(new Variable("i"), new Literal(2))))
      val condition = new LessThan(new Variable("i"), new Literal(2))
      val body = Seq(new Ternary(condition, new Literal(1), new Addition(recursiveCall1, recursiveCall2)))
      val fibonacciMethod = new JavaMethod("fibonacci", JavaType.IntType, parameters, body)
      val methods = Seq(fibonacciMethod)
      val program = new JavaClass(methods)
      program
    }

    val parameters = Seq(parameter("i","Integer"))
    val recursiveCall1 = call(variable("fibonacci"), Seq(subtraction(variable("i"), literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(subtraction(variable("i"), literal(2))))
    val condition = new MetaObject("LessThan") {
      data.put("first", variable("i"))
      data.put("second", literal(2))
    }
    val body = Seq(new MetaObject(classOf[Ternary].getSimpleName) {
      data.put("condition", condition)
      data.put("trueResult", literal(1))
      data.put("falseResult", new MetaObject("Addition") {
        data.put("first", recursiveCall1)
        data.put("second", recursiveCall2)
      })
    })
    val methods = Seq(method("fibonacci","Integer",parameters, body))
    val expectedFibonacciMeta = new MetaObject(classOf[JavaClass].getSimpleName) {
      data.put("methods",methods)
    }
    val fibonacciMeta = JavaASTToMetaProgramConversions.toMetaProgram(fibonacciTyped)
    Assert.assertTrue(MetaObject.deepEquality(fibonacciMeta,expectedFibonacciMeta))
  }
}
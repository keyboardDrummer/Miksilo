package javaInterpreter

import transformation.MetaObject
import util.TypedToMetaProgramConverter
import org.junit.{Assert, Test}
import languages.bytecode._
import languages.javac.base.{JavaMethodModel, JavaBaseModel}
import JavaBaseModel._
import JavaMethodModel._
import languages.javac.base.JavaTypes.IntegerType
import languages.javac._
import javaInterpreter.Call
import javaInterpreter.Variable
import javaInterpreter.JavaParameter
import javaInterpreter.JavaMethod
import javaInterpreter.JavaClass
import javaInterpreter.JavaSubtraction
import javaInterpreter.Literal
import javaInterpreter.Addition
import javaInterpreter.LessThan
import javaInterpreter.Ternary

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

    val parameters = Seq(parameter("i",IntegerType))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = LessThanC.lessThan(variable("i"),LiteralC.literal(2))
    val body = Seq(TernaryC.ternary(condition,
      LiteralC.literal(1),
      AdditionC.addition(recursiveCall1, recursiveCall2)
    ))
    val methods = Seq(method("fibonacci",IntegerType,parameters, body))
    val expectedFibonacciMeta = new MetaObject(classOf[JavaClass].getSimpleName) {
      data.put("methods",methods)
    }
    val fibonacciMeta = JavaASTToMetaProgramConversions.toMetaProgram(fibonacciTyped)
    Assert.assertTrue(MetaObject.deepEquality(fibonacciMeta,expectedFibonacciMeta))
  }
}
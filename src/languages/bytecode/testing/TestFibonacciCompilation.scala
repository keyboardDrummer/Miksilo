package languages.bytecode.testing

import org.junit.{Assert, Test}
import languages.bytecode._
import transformation.{TransformationManager, MetaObject}
import javaInterpreter.{JavaClass, Ternary}
import JavaBase._
import javaInterpreter.Ternary
import util.TestConsole
import javaBytecode.{ByteCodeTypedUnTypedConversions, JavaByteCodeMachine}

class TestFibonacciCompilation {
  @Test
  def test() {
    val fibonacci: MetaObject = getFibonacciMethod
    val compiler = JavaCompiler.getCompiler
    val byteCode = compiler.compile(fibonacci)
    val console = new TestConsole
    val machine = new JavaByteCodeMachine(console)
    val typedByteCode = ByteCodeTypedUnTypedConversions.toTyped(byteCode)
    machine.run(typedByteCode)
    Assert.assertEquals("8",console.stdOut.toString())
  }
  
  def getClazz = {
    JavaBase.clazz(Seq(getMainMethod, getFibonacciMethod))
  }

  def getMainMethod: MetaObject = {
    val parameters = Seq(parameter("args", JavaBase.arrayType(StringType)))
    val fibCall = call(variable("fibonacci"), Seq(LiteralC.literal(5)))
    val body = Seq(call(variable("Console.printf"), Seq(StringLiteral.literal("%i"), fibCall)))
    method("main",VoidType, parameters, body, true, publicVisibility)
  }

  def getFibonacciMethod: MetaObject = {
    val parameters = Seq(parameter("i", "Integer"))
    val recursiveCall1 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(1))))
    val recursiveCall2 = call(variable("fibonacci"), Seq(SubtractionC.subtraction(variable("i"), LiteralC.literal(2))))
    val condition = new MetaObject("LessThan") {
      data.put("first", variable("i"))
      data.put("second", LiteralC.literal(2))
    }
    val body = Seq(new MetaObject(classOf[Ternary].getSimpleName) {
      data.put("condition", condition)
      data.put("trueResult", LiteralC.literal(1))
      data.put("falseResult", AdditionC.addition(recursiveCall1, recursiveCall2))
    })
    method("fibonacci", IntegerType, parameters, body, true)
  }
}

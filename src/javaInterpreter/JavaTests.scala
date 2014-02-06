package javaInterpreter

import _root_.util.TestConsole
import transformation.MetaObject
import org.junit.Test
import javaBytecode.{ClassFile, ByteCodeASTToMetaProgramConversions}
import junit.framework.Assert

class JavaTests {
  def compileJava(program: MetaObject): MetaObject = ???

  @Test
  def testCompileJava() {
    val javaProgram = JavaASTToMetaProgramConversions.toMetaProgram(JavaExamples.fibonacciProgram)
    val byteCodeMeta = compileJava(javaProgram)
    val byteCodeAST = ByteCodeASTToMetaProgramConversions.toAST(byteCodeMeta)
    val console = new TestConsole()
    runByteCodeOnConsole(byteCodeAST, console)
    Assert.assertEquals(8, console.stdOut.toString())
  }

  def runByteCodeOnConsole(byteCode: ClassFile, console: TestConsole) = ???
}

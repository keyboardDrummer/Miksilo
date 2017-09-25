package transformations.javac.statement

import org.scalatest.FunSuite
import transformations.javac.JavaCompiler
import transformations.javac.statements.JavaGotoC
import util.{CompilerBuilder, TestUtils}

class TestJavaGoto extends FunSuite {

  test("basic") {
    val testUtils: TestUtils = new TestUtils(CompilerBuilder.build(Seq(JavaGotoC) ++ JavaCompiler.javaCompilerTransformations))
    assertResult("11")(testUtils.compileAndRun("JavaGoto"))
  }
}

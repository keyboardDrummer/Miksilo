package deltas.javac.statement

import org.scalatest.FunSuite
import deltas.javac.JavaCompilerDeltas
import deltas.javac.statements.JavaGotoDelta
import util.{CompilerBuilder, TestUtils}

class TestJavaGoto extends FunSuite {

  test("basic") {
    val testUtils: TestUtils = new TestUtils(CompilerBuilder.build(Seq(JavaGotoDelta) ++ JavaCompilerDeltas.javaCompilerDeltas))
    assertResult("11")(testUtils.compileAndRun("JavaGoto"))
  }
}

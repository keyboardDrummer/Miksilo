package deltas.javac.expressions

import core.deltas.Delta
import deltas.ClearPhases
import deltas.javac.JavaLanguage
import util.{SourceUtils, TestLanguageBuilder, TestUtils}

class JavaClassTypeTest extends TestUtils(TestLanguageBuilder.build(
    Delta.spliceAndFilterTop(
        JavaLanguage.javaCompilerDeltas,
        JavaLanguage.javaClassSkeleton,
        Seq(ClearPhases)))) {

  test("empty class") {
    val program =
      """class Test { }""".stripMargin
    compile(program)
  }

  test("empty main method") {
    val program =
      """class Test {
        |  public static void main(String[] args) {
        |  }
        |}""".stripMargin
    compile(program)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    compile(program)
  }
}

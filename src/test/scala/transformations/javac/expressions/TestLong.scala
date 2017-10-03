package transformations.javac.expressions

import org.scalatest.FunSuite
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.JavaCompiler
import util.{CompilerBuilder, TestUtils}

class TestLong extends FunSuite {

  test("simpleLong") {
    TestUtils.compareWithJavacAfterRunning("SimpleLong")
  }

  test("longWithoutPoptimize") {
    val regularParticles = JavaCompiler.javaCompilerTransformations
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeC)
    new TestUtils(CompilerBuilder.build(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
  }

  test("overloadedLongMethod") {
    val program =
      """class Fibonacci
        |{
        |    public static void main(java.lang.String[] args)
        |    {
        |        System.out.print(increment(1));
        |        System.out.print(increment(1l));
        |    }
        |
        |    public static long increment(long value)
        |    {
        |        return 1l + value;
        |    }
        |
        |    public static int increment(int value)
        |    {
        |        return 2 + value;
        |    }
        |}""".stripMargin

    val regularParticles = JavaCompiler.javaCompilerTransformations
    TestUtils.compareWithJavacAfterRunning(TestUtils.toFile(program))
  }

}

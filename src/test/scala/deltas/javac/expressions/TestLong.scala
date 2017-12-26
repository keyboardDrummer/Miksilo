package deltas.javac.expressions

import org.scalatest.FunSuite
import deltas.bytecode.additions.PoptimizeDelta
import deltas.javac.JavaCompilerDeltas
import util.{CompilerBuilder, TestUtils}

class TestLong extends FunSuite {

  test("simpleLong") {
    TestUtils.compareWithJavacAfterRunning("SimpleLong")
  }

  test("longWithoutPoptimize") {
    val regularParticles = JavaCompilerDeltas.javaCompilerDeltas
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeDelta)
    new TestUtils(CompilerBuilder.build(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
  }

  test("overloadedLongMethod") {
    val program =
      """class OverloadedLongMethod
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

    TestUtils.compareWithJavacAfterRunning(TestUtils.toFile("OverloadedLongMethod", program))
  }

}

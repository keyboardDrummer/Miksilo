package deltas.javac.expressions

import org.scalatest.FunSuite
import deltas.bytecode.additions.PoptimizeDelta
import deltas.javac.JavaLanguage
import util.{TestLanguageBuilder, LanguageTest}

class TestLong extends FunSuite {

  test("simpleLong") {
    LanguageTest.compareWithJavacAfterRunning("SimpleLong")
  }

  test("longWithoutPoptimize") {
    val regularParticles = JavaLanguage.javaCompilerDeltas
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeDelta)
    new LanguageTest(TestLanguageBuilder.build(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
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

    LanguageTest.compareWithJavacAfterRunning(LanguageTest.toFile("OverloadedLongMethod", program))
  }

}

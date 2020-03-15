package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.deltas.bytecode.additions.PoptimizeDelta
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import util.{JavaLanguageTest, LanguageTest, TestLanguageBuilder}

class TestLong extends JavaLanguageTest {

  test("simpleLong") {
    compareWithJavacAfterRunning("SimpleLong")
  }

  test("longWithoutPoptimize") {
    val regularParticles = JavaToByteCodeLanguage.javaCompilerDeltas
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeDelta)
    new LanguageTest(TestLanguageBuilder.buildWithParser(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
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

    compareWithJavacAfterRunning(toFile("OverloadedLongMethod", program))
  }

}

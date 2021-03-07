package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class BreakTest extends AnyFunSuite {

  test("BreakInWhile") {
    val program =
      """class BreakInWhile {
        |  public static void main(String[] args) {
        |    int x = 0;
        |    while(x < 3) {
        |      if (x == 2)
        |        break;
        |      x += 1;
        |    }
        |    System.out.print(x);
        |  }
        |}
      """.stripMargin
    JavaLanguageTest.compareWithJavacAfterRunning("BreakInWhile", program)
  }

  test("BreakInForLoop") {
    val program =
      """class BreakInWhile {
        |  public static void main(String[] args) {
        |    int x;
        |    for(x = 0; x < 3; x += 1) {
        |      if (x == 2)
        |        break;
        |    }
        |    System.out.print(x);
        |  }
        |}
      """.stripMargin
    JavaLanguageTest.compareWithJavacAfterRunning("BreakInWhile", program)
  }
}

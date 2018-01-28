package deltas.javac.expressions

import deltas.javac.JavaCompilerDeltas
import deltas.javac.expressions.additive.AdditionDelta
import deltas.javac.expressions.literals.{IntLiteralDelta, LongLiteralDelta}
import org.scalatest.FunSuite
import util.{TestLanguageBuilder, TestUtils}

class ExpressionTypeTest
  extends TestUtils(TestLanguageBuilder.build(Seq(ExpressionLanguageDelta) ++ JavaCompilerDeltas.javaSimpleExpression)) {

  test("int + int") {
    val program = "3 + 2"
    language.

  }

  test("int + long") {
    val program = "3 + 2l"
  }
}

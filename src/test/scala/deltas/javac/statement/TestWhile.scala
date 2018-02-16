package deltas.javac.statement

import core.bigrammar.TestCompilerGrammarUtils
import core.deltas.grammars.KeyGrammar
import org.scalatest.FunSuite
import deltas.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import deltas.bytecode.types.VoidTypeDelta.Shape
import util.TestUtils

class TestWhile extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}

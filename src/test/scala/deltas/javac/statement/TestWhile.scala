package deltas.javac.statement

import core.bigrammar.TestCompilerGrammarUtils
import core.particles.grammars.KeyGrammar
import org.scalatest.FunSuite
import deltas.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import deltas.bytecode.types.VoidTypeC.VoidTypeKey
import util.TestUtils

class TestWhile extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}

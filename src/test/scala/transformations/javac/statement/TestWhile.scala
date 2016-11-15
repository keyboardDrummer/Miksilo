package transformations.javac.statement

import core.bigrammar.TestGrammarUtils
import core.particles.grammars.KeyGrammar
import org.scalatest.FunSuite
import transformations.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import transformations.bytecode.types.VoidTypeC.VoidTypeKey
import util.TestUtils

class TestWhile extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}

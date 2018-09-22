package deltas.javac.statement

import core.bigrammar.TestLanguageGrammarUtils
import core.deltas.grammars.KeyGrammar
import org.scalatest.FunSuite
import deltas.bytecode.types.TypeSkeleton.ByteCodeTypeGrammar
import deltas.bytecode.types.VoidTypeDelta.Shape
import util.LanguageTest

class TestWhile extends FunSuite {

  test("basic") {
    LanguageTest.compareWithJavacAfterRunning("Whilee")
  }
}

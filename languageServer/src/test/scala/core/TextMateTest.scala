package core

import org.scalatest.FunSuite
import core.bigrammar.textMate.BiGrammarToTextMate
import _root_.deltas.smithy.SmithyLanguage

class TextMateTest extends FunSuite {
  test("smithy test") {
    val smithy = SmithyLanguage.language
    var output = BiGrammarToTextMate.toTextMate(smithy.grammars.root)
    val addition = """  "information_for_contributors": [],
                     |  "version": "",
                     |  "name": "Smithy",
                     |  "scopeName": "source.smithy",
                     |  """.stripMargin
    val output2 = output.replaceFirst("\n  ", "\n" + addition)
    assertResult("")(output)
  }
}

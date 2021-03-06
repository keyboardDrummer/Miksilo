package miksilo.modularLanguages.deltas.json

import miksilo.languageServer.server.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite

class JsonServerTest extends AnyFunSuite with LanguageServerTest {

  // Currently hangs when passing the whole suite. Maybe moving to uTest will resolve that.
  ignore("regression2") {
    val input = """[]"""

    val jsonLanguage = ModularJsonLanguage.language
    val server = new MiksiloLanguageServer(jsonLanguage)

    val (results0, document) = this.openAndCheckDocument(server, input)
    assert(results0.isEmpty)
    val results1 = applyChange(server, document, 1,1, "{") // [{]
    val results2 = applyChange(server, document, 2,2, "}") // [{}]
    assert(results2.isEmpty)
    val results3 = applyChange(server, document, 2,3, "")  // [{]
    val results4 = applyChange(server, document, 2,2, "}") // [{}]
    assert(results4.isEmpty)
  }
}

package deltas.json

import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite

class JsonServerTest extends AnyFunSuite with LanguageServerTest {

  test("regression2") {
    val input = """[]"""  // [1,2,3,4]     [1,2,[],3]     [{"name":"Remy", age: 31}, {"name":"Tzeni",age:29}]

    val jsonLanguage = JsonLanguage.language
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

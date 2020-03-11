package deltas.yaml

import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite

class YamlServerTest extends AnyFunSuite with LanguageServerTest {

  test("regression") {
    val input =
      """Foo:
        | Bar: baz
        | Yar: yoo
        |""".stripMargin

    val jsonLanguage = YamlLanguage.language
    val server = new MiksiloLanguageServer(jsonLanguage)

    val (results0, document) = this.openAndCheckDocument(server, input)
    assert(results0.isEmpty)
    val results1 = applyChange(server, document, 0,0, " ") // [{]
    assert(results1.nonEmpty)
  }
}

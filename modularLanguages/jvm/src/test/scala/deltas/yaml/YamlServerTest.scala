package deltas.yaml

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import core.parsers.editorParsers.{NeverStop, UntilBestAndXStepsStopFunction, UntilTimeStopFunction}
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite

class YamlServerTest extends AnyFunSuite with LanguageServerTest {

  val yamlLanguage = LanguageFromDeltas(Seq(ParseUsingTextualGrammar(UntilBestAndXStepsStopFunction(), indentationSensitive = true)) ++ YamlLanguage.deltasWithoutParser)
  test("regression") {
    val input =
      """Foo:
        | Bar: baz
        | Yar: yoo
        |""".stripMargin

    val server = new MiksiloLanguageServer(yamlLanguage)

    val (results0, document) = this.openAndCheckDocument(server, input)
    assert(results0.isEmpty)
    val results1 = applyChange(server, document, 0,0, " ")
    assert(results1.nonEmpty)
  }

  test("regression 2") {
    val input = "- Bar:\n   \n- 2"
    val server = new MiksiloLanguageServer(yamlLanguage)

    val (results0, document) = this.openAndCheckDocument(server, input)
    assert(results0.size == 1 && results0.head.message.contains("expected '<value>'"))
    val results1 = applyChange(server, document, 10,10, "Joo")
    assert(results1.isEmpty)
  }
}

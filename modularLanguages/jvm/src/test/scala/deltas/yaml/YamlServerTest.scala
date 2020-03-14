package deltas.yaml

import core.deltas.{LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.editorParser.parsers.editorParsers.{NeverStop, UntilBestAndXStepsStopFunction}
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite

class YamlServerTest extends AnyFunSuite with LanguageServerTest {

  val yamlLanguage = LanguageFromDeltas(Seq(ParseUsingTextualGrammar(UntilBestAndXStepsStopFunction(), indentationSensitive = true)) ++ ModularYamlLanguage.deltasWithoutParser)
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
    // TODO fix plain scalar so we can swap the following two asserts.
    assert(results0.isEmpty)
    //assert(results0.size == 1 && results0.head.message.contains("expected '<value>'"))
    val results1 = applyChange(server, document, 10,10, "Joo")
    assert(results1.isEmpty)
  }

  test("regression 3") {
    val input = """Foo:
                  |  Bar: 3
                  |Faz:
                  |  Far""".stripMargin
    val server = new MiksiloLanguageServer(yamlLanguage)

    val (results0, document) = this.openAndCheckDocument(server, input)
    assert(results0.isEmpty)
    val results1 = applyChange(server, document, 12,13, "")
    assert(results1.nonEmpty)
    val results2 = applyChange(server, document, 12,12, "3")
    assert(results2.isEmpty)
  }
}

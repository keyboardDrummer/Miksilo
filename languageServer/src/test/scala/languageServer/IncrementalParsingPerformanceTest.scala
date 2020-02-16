package languageServer

import core.{SourceUtils, TestUtils}
import core.language.Language
import core.parsers.{ParseJson, PerformanceTest}
import core.parsers.ParseJson.jsonParser
import core.parsers.PerformanceTest.manyRepetitionsTargetTime
import core.parsers.editorParsers.{Position, SourceRange}
import lsp.{DidChangeTextDocumentParams, HumanPosition, LanguageClient, PublishDiagnostics, TextDocumentChangeEvent, TextDocumentContentChangeEvent, TextDocumentItem, VersionedTextDocumentIdentifier}
import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingPerformanceTest extends AnyFunSuite with LanguageServerTest {

  import PerformanceTest._
  import ParseJson._

  val server = new MiksiloLanguageServer(JsonLanguage)
  val jsonParser2 = jsonParser.getWholeInputParser

  test("Make small edit performance") {

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")

    val document = new TextDocumentItem("item","language", 0L, source)
    val documentId = VersionedTextDocumentIdentifier(document.uri, document.version)
    server.didOpen(document)
    val position = HumanPosition(445, 35)
    val positionPlusOne = HumanPosition(445, 36)
    var diagnostics: PublishDiagnostics = null
    val client = new LanguageClient {
      override def sendDiagnostics(newDiagnostics: PublishDiagnostics): Unit = diagnostics = newDiagnostics

      override def trackMetric(name: String, value: Double): Unit = {}
    }
    server.setClient(client)

    TestUtils.runPerformanceTest(20, 100, () => {
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, position)), None, "2"))))
      assert(diagnostics.diagnostics.isEmpty)
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, positionPlusOne)), None, ""))))
      assert(diagnostics.diagnostics.isEmpty)
    })
  }
}

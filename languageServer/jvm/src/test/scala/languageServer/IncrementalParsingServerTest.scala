package languageServer

import core.parsers.editorParsers.SourceRange
import lsp._
import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingServerTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JsonLanguage)

  test("regression") {

    val program = """{
                    |  "a" : {
                    |    "b" : "c",
                    |    "d" : {
                    |      "e" : "f"
                    |    }
                    |  }
                    |}""".stripMargin

    val insert = """
                   |    "b" : "c",
                   |    "d" : {
                   |      "e" : "f"
                   |    }""".stripMargin

    val document = new TextDocumentItem("item","language", 0L, program)
    val documentId = VersionedTextDocumentIdentifier(document.uri, document.version)
    server.didOpen(document)
    val start = HumanPosition(2, 10)
    val end = HumanPosition(6, 6)
    var diagnostics: PublishDiagnostics = null
    val client = new LanguageClient {
      override def sendDiagnostics(newDiagnostics: PublishDiagnostics): Unit = diagnostics = newDiagnostics

      override def trackMetric(name: String, value: Double): Unit = {}
    }
    server.setClient(client)
    server.didChange(DidChangeTextDocumentParams(documentId,
      Seq(TextDocumentContentChangeEvent(Some(SourceRange(start, end)), None, ""))))

    assert(diagnostics.diagnostics.isEmpty)
    diagnostics = null
    server.didChange(DidChangeTextDocumentParams(documentId,
      Seq(TextDocumentContentChangeEvent(Some(SourceRange(start, start)), None, insert))))

    assert(diagnostics.diagnostics.isEmpty)
  }
}

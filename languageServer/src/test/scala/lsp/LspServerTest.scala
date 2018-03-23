package lsp

import core.language.Language
import core.language.node.SourceRange
import langserver.types._

trait LspServerTest {

  val document = new TextDocumentIdentifier("jo")
  val documentItem = new TextDocumentItem("jo","x",1,"")
  class ConnectionFromString(program: String) extends Connection {
    override def setServer(languageServer: LanguageServer): Unit = {
      notifySubscribers(DidOpenTextDocumentParams(documentItem))
      notifySubscribers(DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier(document.uri, 1), Seq(
        TextDocumentContentChangeEvent(None, None, program)
      )))
    }
  }

  def getDefinitionResultForProgram(language: Language, program: String, position: Position): SourceRange = {
    val server = new MiksiloLanguageServer(language, new ConnectionFromString(program))
    val result = server.gotoDefinitionRequest(document, position)
    val range = result.params.head.range
    SourceRange(range.start.asInstanceOf[Position], range.end.asInstanceOf[Position])
  }

  def getCompletionResultForProgram(language: Language, program: String, position: Position): CompletionList = {
    val server = new MiksiloLanguageServer(language, new ConnectionFromString(program))
    server.completionRequest(document, position)
  }
}

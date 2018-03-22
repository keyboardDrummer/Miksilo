package cloudformation

import core.language.Language
import core.language.node.SourceRange
import deltas.cloudformation.CloudFormationLanguage
import langserver.types._
import lsp._
import org.scalatest.FunSuite
import util.SourceUtils

class CloudFormationTest extends FunSuite {

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

  test("Goto definition") {
    val program = SourceUtils.getTestFileContents("CloudFormationParameterReference.stpl")
    val indexDefinition = getDefinitionResultForProgram(CloudFormationLanguage.language, program, new Position(46, 24))
    assertResult(SourceRange(new Position(5,5), new Position(5,25)))(indexDefinition)
  }
}

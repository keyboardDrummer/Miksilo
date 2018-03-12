package deltas.javac.expressions

import core.deltas.Delta
import core.language.Language
import core.language.node.SourceRange
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import langserver.messages.{DidChangeTextDocumentParams, DidOpenTextDocumentParams}
import langserver.types._
import lsp.{Connection, LanguageServer, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class GotoDefinitionTest extends FunSuite {

  private val blockLanguage = Delta.buildLanguage(Seq(DropPhases(1), BlockLanguageDelta) ++ JavaLanguage.blockWithVariables)

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

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val result = getDefinitionResultForProgram(blockLanguage, program, new Position(2, 1))
    assertResult(SourceRange(new Position(1,5), new Position(1,6)))(result)
  }

  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    val xDefinition = getDefinitionResultForProgram(blockLanguage, program, new Position(4, 3))
    assertResult(SourceRange(new Position(1,5), new Position(1,6)))(xDefinition)

    val yDefinition = getDefinitionResultForProgram(blockLanguage, program, new Position(4, 8))
    assertResult(SourceRange(new Position(3,7), new Position(3,8)))(yDefinition)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new Position(10, 16))
    assertResult(SourceRange(new Position(8,37), new Position(8,42)))(indexDefinition)

    val fibonacciDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new Position(5, 36))
    val methodRange = SourceRange(new Position(8, 23), new Position(8, 32))
    assertResult(methodRange)(fibonacciDefinition)
  }

  test("assignment") {
    val program = SourceUtils.getJavaTestFileContents("FieldAssignment")
    val myFieldDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new Position(11, 9))
    assertResult(SourceRange(new Position(2,9), new Position(2,16)))(myFieldDefinition)
  }
}

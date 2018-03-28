package languageServer

import core.language.Language
import langserver.types._
import languageServer.lsp._
import org.scalatest.FunSuite

import scala.util.Random

trait LanguageServerTest extends FunSuite {

  def gotoDefinition(language: Language, program: String, position: HumanPosition): Seq[Location] = {
    val server = new MiksiloLanguageServer(language)
    gotoDefinition(server, program, position)
  }

  def gotoDefinition(server: LanguageServer, program: String, position: HumanPosition): Seq[Location] = {
    val document = openDocument(server, program)
    server.asInstanceOf[DefinitionProvider].gotoDefinition(DocumentPosition(document, position))
  }

  def complete(server: LanguageServer, program: String, position: HumanPosition): CompletionList = {
    val document = openDocument(server, program)
    server.asInstanceOf[CompletionProvider].complete(DocumentPosition(document, position))
  }

  val random = new Random()
  def openDocument(server: LanguageServer, content: String): TextDocumentIdentifier = {
    val item = new TextDocumentItem(random.nextString(10), "", 1, content)
    server.didOpen(item)
    TextDocumentIdentifier(item.uri)
  }
}

package languageServer

import miksilo.editorParser.parsers.editorParsers.{Position, SourceRange}
import miksilo.lspprotocol.lsp.TextDocumentContentChangeEvent
import org.scalatest.funsuite.AnyFunSuite

class InMemoryTextDocumentTest extends AnyFunSuite {

  test("full sync") {
    val uri = "foo"

    val first =
      """it all starts with nothing
        |absolutely nothing""".stripMargin
    val second =
      """but then comes
        |the rest""".stripMargin
    var document = new InMemoryTextDocument(uri, first)

    val change = new TextDocumentContentChangeEvent(None, None, second)

    assert(document.mkString == first)
    document.applyChanges(Seq(change))
    assert(document.mkString == second)
  }

  test("add lines") {
    val uri = "foo"

    val start =
      """once upon a time
        |there was a brave fresh
        |piece of text.""".stripMargin
    val document = new InMemoryTextDocument(uri, start)

    val change1 = new TextDocumentContentChangeEvent(Some(SourceRange(Position(1, 12), Position(1,23))), None,
      """weak,
        |insolent""".stripMargin)
    val change2 = new TextDocumentContentChangeEvent(Some(SourceRange(Position(3, 13), Position(3,14))), None,
      s""",${InMemoryTextDocument.newLine}who couldn't stop.""")

    val end =
      """once upon a time
        |there was a weak,
        |insolent
        |piece of text,
        |who couldn't stop.""".stripMargin
    assert(document.mkString == start)
    document.applyChanges(Seq(change1, change2))
    assert(document.mkString == end)
  }

  test("add blank lines") {
    val uri = "foo"

    val start =
      """
        |
        |
        |""".stripMargin
    val document = new InMemoryTextDocument(uri, start)

    val change1 = new TextDocumentContentChangeEvent(Some(SourceRange(Position(1, 0), Position(1,0))), None, "\n")

    val end =
      """
        |
        |
        |
        |""".stripMargin
    assert(document.mkString == start)
    document.applyChanges(Seq(change1))
    assert(document.mkString == end)
  }

  test("remove lines") {
    val uri = "foo"

    val start =
      """once upon a time
        |there was a brave fresh
        |piece of text.""".stripMargin
    val document = new InMemoryTextDocument(uri, start)

    val change = new TextDocumentContentChangeEvent(Some(SourceRange(Position(0, 16), Position(1,23))), None,
      """, there was a""".stripMargin)

    val end =
      """once upon a time, there was a
        |piece of text.""".stripMargin
    assert(document.mkString == start)
    document.applyChanges(Seq(change))
    assert(document.mkString == end)
  }

  test("lines out of bounds") {
    val uri = "foo"

    val start =
      """some line""".stripMargin
    val document = new InMemoryTextDocument(uri, start)

    val change = new TextDocumentContentChangeEvent(Some(SourceRange(Position(5, 16), Position(5,23))), None,
      """, there was a""".stripMargin)

    document.applyUnsafeChanges(Seq(change))
    assert(true)
  }

  test("character out of bounds") {
    val uri = "foo"

    val start =
      """some line""".stripMargin
    val document = new InMemoryTextDocument(uri, start)

    val change = new TextDocumentContentChangeEvent(Some(SourceRange(Position(0, 16), Position(0,23))), None,
      """, there was a""".stripMargin)

    document.applyUnsafeChanges(Seq(change))
    assert(true)
  }
}

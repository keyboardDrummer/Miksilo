package core.parsers

import org.scalatest.FunSuite
import editorParsers.UnambiguousEditorParserWriter
import strings.DefaultIndentationSensitiveWriter

class IndentationParserTest extends FunSuite with UnambiguousEditorParserWriter
  with DefaultIndentationSensitiveWriter {

  lazy val expression = lazyParser(block | anA)
  val whiteSpace = RegexParser("""\s*""".r)
  val anA: EditorParser[Any] = "a" ~< whiteSpace
  val block: EditorParser[Any] = WithIndentation("block:" ~< whiteSpace ~ greaterThan(alignedList(expression)))

  test("basic") {
    val program =
      """block:
        | a
        | block:
        |  a
        |  a
        | a
      """.stripMargin

    val result = expression.parseWholeInput(new IndentationReader(program))
    assert(result.successful)
  }
}

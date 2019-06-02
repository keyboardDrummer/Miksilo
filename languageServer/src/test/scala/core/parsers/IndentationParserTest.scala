package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import strings.DefaultIndentationSensitiveWriter

class IndentationParserTest extends FunSuite with LeftRecursiveCorrectingParserWriter
  with DefaultIndentationSensitiveWriter {

  lazy val expression = new Lazy(block | anA)
  val whiteSpace = RegexParser("""\s*""".r, "whitespace")
  val anA: Self[Any] = "a" ~< whiteSpace
  val block: Self[Any] = WithIndentation("block:" ~< whiteSpace ~ greaterThan(alignedList(expression)))

  test("basic") {
    val program =
      """block:
        | a
        | block:
        |  a
        |  a
        | a
      """.stripMargin

    val result = expression.getWholeInputParser()(new IndentationReader(program))
    assert(result.successful)
  }
}

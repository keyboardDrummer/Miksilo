package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import strings.DefaultIndentationSensitiveWriter

class IndentationParserTest extends AnyFunSuite with LeftRecursiveCorrectingParserWriter
  with DefaultIndentationSensitiveWriter {

  lazy val expression = new Lazy(block | anA)
  val whiteSpace = RegexParser("""\s*""".r, "whitespace")
  val anA: Parser[Any] = "a" ~< whiteSpace
  val block: Parser[Any] = WithIndentation("block:" ~< whiteSpace ~ greaterThan(alignedList(expression)))

  test("basic") {
    val program =
      """block:
        | a
        | block:
        |  a
        |  a
        | a
      """.stripMargin

    val result = expression.getWholeInputParser.parse(new IndentationReader(program))
    assert(result.successful)
  }
}

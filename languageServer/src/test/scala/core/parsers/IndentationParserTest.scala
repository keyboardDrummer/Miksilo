package core.parsers

import org.scalatest.FunSuite
import editorParsers.UnambiguousEditorParserWriter
import langserver.types.Position
import strings.IndentationSensitiveParserWriter

class IndentationParserTest extends FunSuite with UnambiguousEditorParserWriter
  with IndentationSensitiveParserWriter {

  type Input = StringReader
  case class StringReader(array: ArrayCharSequence, offset: Int, position: Position, indentation: Int) extends IndentationReaderLike {

    def this(value: String) {
      this(value.toCharArray, 0, Position(0, 0), 0)
    }

    val sequence: CharSequence = array
    def drop(amount: Int): StringReader = StringReader(array, offset + amount,
      newPosition(position, array, offset, amount), indentation)

    override def atEnd: Boolean = offset == array.length

    override def head: Char = array.charAt(offset)

    override def tail: StringReader = drop(1)

    override def hashCode(): Int = offset ^ indentation

    override def equals(obj: Any): Boolean = obj match {
      case other: StringReader => offset == other.offset && indentation == other.indentation
      case _ => false
    }

    override def toString: String = {
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
    }

    override def withIndentation(value: Int) = StringReader(array, offset, position, value)
  }

  lazy val expression = lazyParser(block | anA)
  val whiteSpace = RegexParser("""\s*""".r)
  val anA: EditorParser[Any] = "a" ~< whiteSpace
  val block: EditorParser[Any] = WithIndentation("block:" ~< whiteSpace ~
    greaterThan(aligned(expression, List.empty, (a: Any, b: List[Any]) => a :: b)))

  test("basic") {
    val program =
      """block:
        | a
        | block:
        |  a
        |  a
        | a
      """.stripMargin

    val result = expression.parse(new StringReader(program))
    assert(result.successful)
  }
}

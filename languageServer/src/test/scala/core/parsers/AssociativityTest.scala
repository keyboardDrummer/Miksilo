package core.parsers

import org.scalatest.FunSuite
import editorParsers.EditorParserWriter
import strings.CommonParserWriter

import scala.util.parsing.input.OffsetPosition

trait CommonStringReaderParser extends CommonParserWriter with EditorParserWriter {
  type Input = StringReader

  case class StringReader(array: ArrayCharSequence, offset: Int = 0) extends StringReaderLike {

    def this(value: String) {
      this(value.toCharArray)
    }

    val sequence: CharSequence = array
    def drop(amount: Int): StringReader = StringReader(array, offset + amount)
    lazy val position = OffsetPosition(sequence, offset)

    override def atEnd: Boolean = offset == array.length

    override def head: Char = array.charAt(offset)

    override def tail: StringReader = drop(1)

    override def hashCode(): Int = offset

    override def equals(obj: Any): Boolean = obj match {
      case other: StringReader => offset == other.offset
      case _ => false
    }

    override def toString: String = {
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
    }
  }
}

trait AssociativityTest extends FunSuite with CommonStringReaderParser with EditorParserWriter {

  test("binary operators are right associative by default") {
    lazy val expr: EditorParser[Any] = new EditorLazy(expr) ~< "-" ~ expr | wholeNumber
    val input = "1-2-3"
    val result = expr.parseWholeInput(new StringReader(input))
    assert(result.successful)
    assertResult(("1",("2","3")))(result.get)
  }

  test("binary operators can be made left associative") {
    lazy val expr: EditorParser[Any] = wholeNumber.addAlternative[Any]((before, after) => after ~< "-" ~ before)
    val input = "1-2-3"
    val result = expr.parseWholeInput(new StringReader(input))
    assert(result.successful)
    assertResult((("1","2"),"3"))(result.get)
  }

  test("binary operators can be explicitly right associative") {
    lazy val expr: EditorParserExtensions[Any] = wholeNumber.addAlternative[Any]((before, after) => before ~< "-" ~ after)
    val input = "1-2-3"
    val result = expr.parseWholeInput(new StringReader(input))
    assert(result.successful)
    assertResult(("1",("2","3")))(result.get)
  }

  test("if-then-else can be made right-associative") {
    lazy val expr = wholeNumber
    val stmt: EditorParser[Any] = expr.
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after ~ "else" ~ before).
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after)
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(result.successful)
    val nestedIf = (((("2", "then"), "3"), "else"), "4")
    assertResult((("1","then"),nestedIf))(result.get)

    val input2 = "if1thenif2then3else4else5"
    val result2 = stmt.parseWholeInput(new StringReader(input2))
    assert(result2.successful)

    val input3 = "if1thenif2then3"
    val result3 = stmt.parseWholeInput(new StringReader(input3))
    assert(result3.successful)
  }
}

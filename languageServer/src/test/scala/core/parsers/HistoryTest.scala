package core.parsers

import langserver.types.Position
import org.scalatest.FunSuite
import strings.StringParserWriter
import editorParsers.SpotlessHistory
import editorParsers.History

class HistoryTest extends FunSuite with StringParserWriter {
  type Input = IndexInput

  test("two consecutive drops combine and become cheaper") {
    val splitDrops = SpotlessHistory().
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addSuccess(History.successValue).
      addError(DropError(new IndexInput(2),new IndexInput(3)))

    val dropsLeft = SpotlessHistory().
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addError(DropError(new IndexInput(1),new IndexInput(2))).
      addSuccess(History.successValue)

    val dropsRight = SpotlessHistory().
      addSuccess(History.successValue).
      addError(DropError(new IndexInput(1),new IndexInput(2))).
      addError(DropError(new IndexInput(2),new IndexInput(3)))

    assert(dropsRight.score == dropsLeft.score)
    assert(splitDrops.score < dropsRight.score)
  }

  test("splitting a long drop with a success is bad") {
    val start = new IndexInput(0)
    val middle = new IndexInput(50)
    val middlePlusOne = new IndexInput(51)
    val end = new IndexInput(100)
    val withoutSuccess = SpotlessHistory().addError(DropError(start, end))
    val withSuccess = SpotlessHistory().
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middlePlusOne, end))

    assert(withSuccess.score < withoutSuccess.score)
  }

  test("splitting a 2 char drop with a success is good") {
    val start = new IndexInput(0)
    val middle = new IndexInput(1)
    val end = new IndexInput(2)
    val withoutSuccess = SpotlessHistory().addError(DropError(start, end))
    val withSuccess = SpotlessHistory().
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middle, end))

    assert(withSuccess.score > withoutSuccess.score)
  }

  test("very long drops have small differences") {
    val start = new IndexInput(0)
    val far = new IndexInput(40)
    var further = new IndexInput(far.offset + 1)
    val farWithoutSuccess = SpotlessHistory().addError(DropError(start, far))
    val furtherWithoutSuccess = SpotlessHistory().addError(DropError(start, further))
    val furtherWithSuccess = furtherWithoutSuccess.addSuccess(History.successValue)
    assert(farWithoutSuccess.score > furtherWithoutSuccess.score)
    assert(farWithoutSuccess.score < furtherWithSuccess.score)
  }

  private val text = Array.fill(100)('a')

  class IndexInput(offset: Int) extends StringReaderBase(text, offset, Position(0,0)) {
    override def drop(amount: Int) = new IndexInput(offset + amount)
  }

  override def wrapParser[Result](parser: Parser[Result], shouldCache: Boolean, shouldDetectLeftRecursion: Boolean) = ???

  override def newParseState(input: IndexInput) = ???

  override type ParseState = this.type
}

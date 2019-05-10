package core.parsers

import langserver.types.Position
import org.scalatest.FunSuite
import strings.StringParserWriter

class HistoryTest extends FunSuite with StringParserWriter {
  type Input = IndexInput

  test("two consecutive drops combine and become cheaper") {
    val splitDrops = new History().
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addTestSuccess(new IndexInput(2)).
      addError(DropError(new IndexInput(2),new IndexInput(3)))

    val dropsLeft = new History().
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addError(DropError(new IndexInput(1),new IndexInput(2))).
      addTestSuccess(new IndexInput(3))

    val dropsRight = new History().
      addTestSuccess(new IndexInput(1)).
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
    val withoutSuccess = new History().addError(DropError(start, end))
    val withSuccess = new History().
      addError(DropError(start, middle)).
      addTestSuccess(middlePlusOne).
      addError(DropError(middlePlusOne, end))

    assert(withSuccess.score < withoutSuccess.score)
  }

  test("splitting a 5 char drop with a success is good") {
    val start = new IndexInput(0)
    val middle = new IndexInput(2)
    val middlePlusOne = new IndexInput(3)
    val end = new IndexInput(5)
    val withoutSuccess = new History().addError(DropError(start, end))
    val withSuccess = new History().
      addError(DropError(start, middle)).
      addTestSuccess(middlePlusOne).
      addError(DropError(middlePlusOne, end))

    assert(withSuccess.score > withoutSuccess.score)
  }

  test("very long drops have small differences") {
    val start = new IndexInput(0)
    val far = new IndexInput(40)
    var further = new IndexInput(far.offset + 1)
    val farWithoutSuccess = new History().addError(DropError(start, far))
    val furtherWithoutSuccess = new History().addError(DropError(start, further))
    val furtherWithSuccess = furtherWithoutSuccess.addTestSuccess(new IndexInput(further.offset + 1))
    assert(farWithoutSuccess.score > furtherWithoutSuccess.score)
    assert(farWithoutSuccess.score < furtherWithSuccess.score)
  }

  private val text = Array.fill(100)('a')

  class IndexInput(offset: Int) extends StringReaderBase(text, offset, Position(0,0)) {
    override def drop(amount: Int) = new IndexInput(offset + amount)
  }

  override def wrapParse[Result](parser: Parse[Result], shouldCache: Boolean, shouldDetectLeftRecursion: Boolean) = ???

  override def newParseState(input: IndexInput) = ???

  override type ParseState = this.type
}

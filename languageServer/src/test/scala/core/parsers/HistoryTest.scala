package core.parsers

import org.scalatest.FunSuite
import strings.StringParserWriter

class HistoryTest extends FunSuite with StringParserWriter {
  type Input = IndexInput

  test("two consecutive drops combine and become cheaper") {
    val splitDrops = new History().
      addError(DropError(IndexInput(0),IndexInput(1), "")).
      addSuccess(IndexInput(2)).
      addError(DropError(IndexInput(2),IndexInput(3), ""))

    val dropsLeft = new History().
      addError(DropError(IndexInput(0),IndexInput(1), "")).
      addError(DropError(IndexInput(1),IndexInput(2), "")).
      addSuccess(IndexInput(3))

    val dropsRight = new History().
      addSuccess(IndexInput(1)).
      addError(DropError(IndexInput(1),IndexInput(2))).
      addError(DropError(IndexInput(2),IndexInput(3)))

    assert(dropsRight.score == dropsLeft.score)
    assert(splitDrops.score < dropsRight.score)
  }

  test("splitting a long drop with a success is good") {
    val start = IndexInput(0)
    val middle = IndexInput(50)
    val middlePlusOne = IndexInput(51)
    val end = IndexInput(100)
    val withoutSuccess = new History().addError(DropError(start, end))
    val furtherWithoutSuccess = new History().
      addError(DropError(start, middle)).
      addSuccess(middlePlusOne).
      addError(DropError(middlePlusOne, end))

    assert(furtherWithoutSuccess.score > withoutSuccess.score)
  }

  test("splitting a short drop with a success is good") {
    val start = IndexInput(0)
    val middle = IndexInput(1)
    val middlePlusOne = IndexInput(2)
    val end = IndexInput(3)
    val withoutSuccess = new History().addError(DropError(start, end))
    val furtherWithoutSuccess = new History().
      addError(DropError(start, middle)).
      addSuccess(middlePlusOne).
      addError(DropError(middlePlusOne, end))

    assert(furtherWithoutSuccess.score > withoutSuccess.score)
  }

  test("very long drops have small differences") {
    val start = IndexInput(0)
    val far = IndexInput(40)
    var further = IndexInput(far.offset + 1)
    val farWithoutSuccess = new History().addError(DropError(start, far, ""))
    val furtherWithoutSuccess = new History().addError(DropError(start, further, ""))
    val furtherWithSuccess = furtherWithoutSuccess.addSuccess(IndexInput(further.offset + 1))
    assert(farWithoutSuccess.score > furtherWithoutSuccess.score)
    assert(farWithoutSuccess.score < furtherWithSuccess.score)
  }

  case class IndexInput(offset: Int) extends StringReaderLike {

    override def atEnd = ???

    override def position = ???

    override def array = ???

    override def drop(amount: Int) = ???

    override def head = ???

    override def tail = ???
  }

  override def wrapParse[Result](parser: Parse[Result], shouldCache: Boolean, shouldDetectLeftRecursion: Boolean) = ???

  override def newParseState(input: IndexInput) = ???

  override type ParseState = this.type
}

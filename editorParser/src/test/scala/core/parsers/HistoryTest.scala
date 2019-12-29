package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import strings.StringParserWriter
import editorParsers.History
import _root_.core.parsers.strings.StringReaderBase
import _root_.core.parsers.editorParsers.Position

class HistoryTest extends AnyFunSuite with StringParserWriter {
  type Input = IndexInput

  test("Dropping , is worse than missing <object member key>:<value>") {
    val commaSpot = new IndexInput(0)
    val afterComma = new IndexInput(1)
    val dropHistory = History.empty.
      addError(DropError(commaSpot, afterComma))

    val missingHistory = History.empty.
      addError(new MissingInput(afterComma, "key")).
      addError(new MissingInput(afterComma, ":")).
      addError(new MissingInput(afterComma, "<value>", penalty = History.insertFallbackPenalty))

    assert(dropHistory.score < missingHistory.score)
  }

  test("Missing ': <value> ,' is better than Missing ': {', Missing '}'") {
    val first = new IndexInput(0)
    val second = new IndexInput(1)
    val noBracesInserted = History.empty.
      addError(new MissingInput(first, ":")).
      addError(new MissingInput(first, "<value>", penalty = History.insertFallbackPenalty)).
      addError(new MissingInput(first, ","))

    val insertedBraces = History.empty.
      addError(new MissingInput(first, ":")).
      addError(new MissingInput(first, "{")).
      addError(new MissingInput(second, "}"))

    assert(insertedBraces.score < noBracesInserted.score)
  }

  test("two consecutive drops combine and become cheaper") {
    val splitDrops = History.empty.
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addSuccess(History.successValue).
      addError(DropError(new IndexInput(2),new IndexInput(3)))

    val dropsLeft = History.empty.
      addError(DropError(new IndexInput(0),new IndexInput(1))).
      addError(DropError(new IndexInput(1),new IndexInput(2))).
      addSuccess(History.successValue)

    val dropsRight = History.empty.
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
    val withoutSuccess = History.empty.addError(DropError(start, end))
    val withSuccess = History.empty.
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middlePlusOne, end))

    assert(withSuccess.score < withoutSuccess.score)
  }

  test("splitting a 2 char drop with a success is good") {
    val start = new IndexInput(0)
    val middle = new IndexInput(1)
    val end = new IndexInput(2)
    val withoutSuccess = History.empty.addError(DropError(start, end))
    val withSuccess = History.empty.
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middle, end))

    assert(withSuccess.score > withoutSuccess.score)
  }

  test("very long drops have small differences") {
    val start = new IndexInput(0)
    val far = new IndexInput(40)
    var further = new IndexInput(far.offset + 1)
    val farWithoutSuccess = History.empty.addError(DropError(start, far))
    val furtherWithoutSuccess = History.empty.addError(DropError(start, further))
    val furtherWithSuccess = furtherWithoutSuccess.addSuccess(History.successValue)
    assert(farWithoutSuccess.score > furtherWithoutSuccess.score)
    assert(farWithoutSuccess.score < furtherWithSuccess.score)
  }

  private val text = Array.fill(100)('a')

  class IndexInput(offset: Int) extends StringReaderBase[IndexInput](text, offset, Position(0,0)) {
    override def drop(amount: Int) = new IndexInput(offset + amount)
  }

  override def wrapParser[Result](parser: BuiltParser[Result], shouldCache: Boolean, shouldDetectLeftRecursion: Boolean) = ???

  override def newParseState(input: IndexInput) = ???

  override type ParseState = this.type
}

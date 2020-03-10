package core.parsers

import _root_.core.parsers.caching.ArrayOffsetManager
import _root_.core.parsers.core.ParseText
import _root_.core.parsers.editorParsers.History
import _root_.core.parsers.strings.{NoStateParserWriter, StringParserWriter}
import org.scalatest.funsuite.AnyFunSuite

class HistoryTest extends AnyFunSuite with StringParserWriter with NoStateParserWriter {
  
  val arrayOffsetManager = new ArrayOffsetManager(new ParseText())
  def pointer(offset: Int) = arrayOffsetManager.getOffsetNode(offset)
  
  test("Dropping , is worse than missing <object member key>:<value>") {
    val commaSpot = pointer(0)
    val afterComma = pointer(1)
    val dropHistory = History.empty.
      addError(DropError(commaSpot, afterComma))

    val missingHistory = History.empty.
      addError(new MissingInput(afterComma, "key")).
      addError(new MissingInput(afterComma, ":")).
      addError(new MissingInput(afterComma, "<value>", penalty = History.insertFallbackPenalty))

    assert(dropHistory.score < missingHistory.score)
  }

  test("Missing ': <value> ,' is better than Missing ': {', Missing '}'") {
    val first = pointer(0)
    val second = pointer(1)
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
      addError(DropError(pointer(0),pointer(1))).
      addSuccess(History.successValue).
      addError(DropError(pointer(2),pointer(3)))

    val dropsLeft = History.empty.
      addError(DropError(pointer(0),pointer(1))).
      addError(DropError(pointer(1),pointer(2))).
      addSuccess(History.successValue)

    val dropsRight = History.empty.
      addSuccess(History.successValue).
      addError(DropError(pointer(1),pointer(2))).
      addError(DropError(pointer(2),pointer(3)))

    assert(dropsRight.score == dropsLeft.score)
    assert(splitDrops.score < dropsRight.score)
  }

  test("splitting a long drop with a success is bad") {
    val start = pointer(0)
    val middle = pointer(50)
    val middlePlusOne = pointer(51)
    val end = pointer(100)
    val withoutSuccess = History.empty.addError(DropError(start, end))
    val withSuccess = History.empty.
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middlePlusOne, end))

    assert(withSuccess.score < withoutSuccess.score)
  }

  test("splitting a 2 char drop with a success is good") {
    val start = pointer(0)
    val middle = pointer(1)
    val end = pointer(2)
    val withoutSuccess = History.empty.addError(DropError(start, end))
    val withSuccess = History.empty.
      addError(DropError(start, middle)).
      addSuccess(History.successValue).
      addError(DropError(middle, end))

    assert(withSuccess.score > withoutSuccess.score)
  }

  test("very long drops have small differences") {
    val start = pointer(0)
    val far = pointer(40)
    val further = pointer(far.offset + 1)
    val farWithoutSuccess = History.empty.addError(DropError(start, far))
    val furtherWithoutSuccess = History.empty.addError(DropError(start, further))
    val furtherWithSuccess = furtherWithoutSuccess.addSuccess(History.successValue)
    assert(farWithoutSuccess.score > furtherWithoutSuccess.score)
    assert(farWithoutSuccess.score < furtherWithSuccess.score)
  }

  //private val text = Array.fill(100)('a')

}

package core.parsers

import org.scalatest.FunSuite
import strings.StringParserWriter
import core.ParseInput

class HistoryTest extends FunSuite with StringParserWriter {
  type Input = IndexInput

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

    override def scoredPosition = ???

    override def array = ???

    override def drop(amount: Int) = ???

    override def head = ???

    override def tail = ???
  }

  override def wrapParse[Result](parser: Parse[Result], shouldCache: Boolean, shouldDetectLeftRecursion: Boolean) = ???

  override def newParseState(input: IndexInput) = ???

  override type ParseState = this.type
}

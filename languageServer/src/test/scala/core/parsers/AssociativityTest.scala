package core.parsers

import core.parsers.strings.StringReader
import org.scalatest.FunSuite

class AssociativityTest extends FunSuite with CommonParserWriter {

  test("binary operators are right associative by default") {
    lazy val expr: Parser[Any] = new Lazy(expr) ~< "-" ~ expr | wholeNumber
    val input = "1-2-3"
    val result = expr.parseWholeInput(new StringReader(input))
    assert(result.successful)
    assertResult(("1",("2","3")))(result.get)
  }
}

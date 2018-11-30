package core.parsers

import org.scalatest.FunSuite
import strings.CommonParserWriter
import strings.StringReader

class AmbiguityTest extends FunSuite with CommonParserWriter {

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

  test("if-then-else is right-associative by default") {
    lazy val expr = wholeNumber
    lazy val stmt: EditorParser[Any] = expr |
      "if" ~> expr ~ "then" ~ stmt ~ "else" ~ stmt |
      "if" ~> expr ~ "then" ~ stmt
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(result.successful)
    val nestedIf = (((("2", "then"), "3"), "else"), "4")
    assertResult((("1","then"),nestedIf))(result.get)
  }

  test("if-then-else can not be made left-associative") { // TODO use parsers with multiple results to allow left-associative if-then-else.
    lazy val expr = wholeNumber
    val stmt: EditorParser[Any] = expr.
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after).
      addAlternative((before, after) => "if" ~> expr ~ "then" ~ before ~ "else" ~ after)
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(result.successful)
    val nestedIf = (("2", "then"), "3")
    assertResult((((("1","then"),nestedIf),"else"),"4"))(result.get)

    val input2 = "if1thenif2then3else4else5"
    val result2 = stmt.parseWholeInput(new StringReader(input2))
    assert(!result2.successful)
  }
}

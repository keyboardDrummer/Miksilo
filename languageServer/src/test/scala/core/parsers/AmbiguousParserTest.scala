package core.parsers

import ambiguousEditorParsers.AmbiguousEditorParserWriter
import strings.StringReader

class AmbiguousParserTest extends AssociativityTest
  with LeftRecursionTest
  with PartiallyParseJsonTest
  with AmbiguousEditorParserWriter
  with ErrorReportingTest {

  test("Basic ambiguity test") {
    lazy val expression: EditorParser[Any] = ("ab" | "a") ~ "bc"
    val result = expression.parseWholeInput(new StringReader("abc"))
    assert(result.successful, result.toString)
  }

  test("Optional before recursive and seed") {
    lazy val expression: EditorParser[Any] = optional ~ expression ~ "s" | optional ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: EditorParser[Any] = optional ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order") {
    lazy val expression: EditorParser[Any] = optional ~ choice("e", expression ~ "s", leftIsAlwaysBigger = true)
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  test("if-then-else is left-associative by default") {
    lazy val expr = wholeNumber
    lazy val stmt: EditorParser[Any] = expr |
      "if" ~> expr ~ "then" ~ stmt ~ "else" ~ stmt |
      "if" ~> expr ~ "then" ~ stmt
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(result.successful)

    val nestedIf = (("2", "then"), "3")
    assertResult((((("1","then"),nestedIf),"else"),"4"))(result.get)
  }

  test("if-then-else can be made left-associative") {
    lazy val expr = wholeNumber
    val stmt: EditorParser[Any] = expr.
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after).
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after ~ "else" ~ after)
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(result.successful)
    val nestedIf = (("2", "then"), "3")
    assertResult((((("1","then"),nestedIf),"else"),"4"))(result.get)

    val input2 = "if1thenif2then3else4else5"
    val result2 = stmt.parseWholeInput(new StringReader(input2))
    assert(result2.successful)
  }

}

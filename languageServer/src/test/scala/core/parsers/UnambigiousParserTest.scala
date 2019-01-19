package core.parsers

import strings.StringReader
import editorParsers.UnambiguousEditorParserWriter

class UnambigiousParserTest extends AssociativityTest
  with LeftRecursionTest
  with UnambiguousEditorParserWriter
  with PartiallyParseJsonTest
  with ErrorReportingTest {

  test("Basic ambiguity test fails") {
    lazy val expression: EditorParser[Any] = ("ab" | "a") ~ "bc"
    val result = expression.parseWholeInput(new StringReader("abc"))
    assert(!result.successful, result.toString)
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

  test("if-then-else can not be made left-associative") {
    lazy val expr = wholeNumber
    val stmt: EditorParser[Any] = expr.
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after).
      addAlternative[Any]((before, after) => "if" ~> expr ~ "then" ~ after ~ "else" ~ after)
    val input = "if1thenif2then3else4"
    val result = stmt.parseWholeInput(new StringReader(input))
    assert(!result.successful)
  }

  test("Optional before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional ~ expression ~ "s" | optional ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order FAILS") {
    lazy val expression: EditorParser[Any] = optional ~ choice("e", expression ~ "s", true)
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }
}

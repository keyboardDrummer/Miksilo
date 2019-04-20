package core.parsers

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

  test("Optional before choice") {
    lazy val expression: EditorParser[Any] = optional_a ~ (expression ~ "s" | "e")
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This one fails in unambiguous parsers
  }

  test("Optional before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optional_a ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ choice("e", expression ~ "s", true)
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  // Partially Parse tests start
  test("object with single member with string value, where the colon is missing") {
    val input = """{"person""remy"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy")))(result.resultOption.get)
  }

  test("two members but the first misses a colon") {
    val input = """{"person""remy","friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members but the comma is missing") {
    val input = """{"person":"remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members both missing colon") {
    val input = """{"person""remy","friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members, no first colon and comma") {
    val input = """{"person""remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members, no colons or comma") {
    val input = """{"person""remy""friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("nested two members without colons/comma") {
    val input = """{"directory"{"person""remy""friend""jeroen"}}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List("directory" -> List("person" -> "remy", "friend" -> "jeroen")))(result.resultOption.get)
  }

  test("starting brace insertion") {
    val input = """{"person""remy":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person",List(("remy","jeroen")))))(result.resultOption.get)
  }

  test("virtual left recursion through error correction") {
    val input = """doesNotMatchGrammar"""
    lazy val parser: EditorParser[Any] = "{" ~ parser | "x"
    val result = parser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult("x")(result.resultOption.get)
  }

  // Add test for left recursion and errors
  // Add test with multiple errors in one branch "b" => "a" "b" "c"
  // Add test with three way branch with 0,1,2 errors, and 0,2,1 errors.

  // Partially Parse tests end
}

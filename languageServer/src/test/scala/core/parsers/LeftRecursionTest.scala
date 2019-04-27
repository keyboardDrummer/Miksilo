package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter

class LeftRecursionTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  val optional_a: EditorParserExtensions[Any] =  literal("a").*
  val optionalCopy: EditorParserExtensions[Any] = literal("a").*
  def aesReader = new StringReader("aes")

  test("left recursion with lazy indirection") {
    lazy val head: EditorParser[Any] = new EditorLazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(parseResult.successful)
    val expectation = (("a","a"),"a")
    assertResult(expectation)(parseResult.get)
  }

  test("handles recursion in complicated graph structures") {
    lazy val leftMayNotCache = leftRec ~ "b"
    lazy val leftRec = leftPath.map(x => x)
    lazy val leftPath: EditorParser[Any] = new EditorLazy(leftMayNotCache | leftRec ~ "a" | "b")

    val input = "bbb"
    val leftParseResult = leftPath.parseWholeInput(new StringReader(input))
    assert(leftParseResult.successful)
    val expectation = (("b","b"),"b")
    assertResult(expectation)(leftParseResult.get)

    lazy val rightMayNotCache = rightRec ~ "b"
    lazy val rightRec = rightPath.map(x => x)
    lazy val rightPath: EditorParser[Any] = new EditorLazy(rightRec ~ "a" | rightMayNotCache | "b")
    val rightParseResult = rightPath.parseWholeInput(new StringReader(input))
    assertResult(leftParseResult)(rightParseResult)
  }

  test("left recursion inside left recursion") {
    lazy val head: EditorParser[Any] = second ~ "a" | second
    lazy val second: EditorParser[Any] = new EditorLazy(second) ~ "b" | head | "c"

    val input = "caabb"
    val expectation = (((("c","a"),"a"),"b"),"b")
    val headParseResult = head.parseWholeInput(new StringReader(input))
    assert(headParseResult.successful)
    assertResult(expectation)(headParseResult.get)

    val secondParseResult = second.parseWholeInput(new StringReader(input))
    assert(secondParseResult.successful)
    assertResult(expectation)(secondParseResult.get)
  }

  test("Optional before seed") {
    lazy val expression: EditorParser[Any] = new EditorLazy(expression) ~ "s" | optional_a ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  /**
   * This fails similarly to (a | ab) ~ bc.
   * The optional causes it to be something like:
   *   expression1 = "a" ~ expression2 ~ "s" | "e"
   *   expression2 = expression2 ~ "s" | "e"
   * The expression2 will parse an "s", even though expression1 still needs to parse "s"
  */
  test("Optional before recursive FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }


  test("Recursive defaults") {
    lazy val recursive: EditorParser[Any] = new EditorLazy(recursive) ~ "b" | "b"
    lazy val parser = "a" ~ recursive
    val input = "c"
    val expectation = ("a", "b")
    val result = parser.parseWholeInput(new StringReader(input))
    assertResult(expectation)(result.resultOption.get)
  }

  // a cycle of lazy parsers causes a stack overflow, since they have no cycle check, but with a sequence in between it just fails.
  test("only recursive with sequence indirection") {
    lazy val first: EditorParser[Any] = new EditorLazy(first) ~ "a"
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("only recursive with sequence indirection and default, " +
    "does not apply the default after failing the recursion") {
    lazy val first: EditorParser[Any] = (new EditorLazy(first) ~ "a").withDefault("yes")
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("recursive with sequence indirection and default, " +
    "applies the default after failing the recursion") {
    lazy val first: EditorParser[Any] = (new EditorLazy(first) ~ "a" | "a").withDefault("yes")
    val input = "notavailable"
    val parseResult = first.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val expectation = Some("yes") //Could have been ("yes","a") with different implementation
    assertResult(expectation)(parseResult.resultOption)
  }
}

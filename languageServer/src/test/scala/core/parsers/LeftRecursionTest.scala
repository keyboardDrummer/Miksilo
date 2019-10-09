package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import org.scalatest.FunSuite
import org.scalatest.concurrent.{TimeLimitedTests, TimeLimits}
import org.scalatest.time.{Millis, Span}

class LeftRecursionTest extends FunSuite with CommonStringReaderParser
  with LeftRecursiveCorrectingParserWriter
  with TimeLimits
{

  val optional_a: SequenceParserExtensions[Any] = Literal("!").*
  val optionalCopy: SequenceParserExtensions[Any] = Literal("!").*
  def aesReader = new StringReader("!#@")

  test("left recursion with lazy indirection") {
    lazy val head: Self[Any] = new Lazy(head) ~ "!" | "!"

    val input = "!!!"
    val parseResult = head.getWholeInputParser.parse(new StringReader(input))
    assert(parseResult.successful)
    val expectation = (("!","!"),"!")
    assertResult(expectation)(parseResult.get)
  }

  test("handles recursion in complicated graph structures") {
    lazy val leftMayNotCache = leftRec ~ "@"
    lazy val leftRec = leftPath.map(x => x)
    lazy val leftPath: Self[Any] = new Lazy(leftMayNotCache | leftRec ~ "!" | "@")

    val input = "@@@"
    val leftParseResult = leftPath.getWholeInputParser.parse(new StringReader(input))
    assert(leftParseResult.successful)
    val expectation = (("@","@"),"@")
    assertResult(expectation)(leftParseResult.get)

    lazy val rightMayNotCache = rightRec ~ "@"
    lazy val rightRec = rightPath.map(x => x)
    lazy val rightPath: Self[Any] = new Lazy(rightRec ~ "!" | rightMayNotCache | "@")
    val rightParseResult = rightPath.getWholeInputParser.parse(new StringReader(input))
    assertResult(leftParseResult)(rightParseResult)
  }

  //[head] second b second b second head a second head a second c
  /*
  De second initialResults is netjes: c | recurse(second) | recursive(head)
  Als die gegrown worden dan krijgen we: c | grow(c) -> ded | grow(recursive(head) -> recursive' | recursive(head)
  Voor head initialResults krijg ik twee paar results voor beide second recursions, en dan per paar..
   */

  test("left recursion inside left recursion") {
    lazy val head: Self[Any] = second ~ "!" | second
    lazy val second: Self[Any] = new Lazy(second, "secondRef") ~ "@" | head | "#"

    val input = "#!!@@"
    val expectation = (((("#","!"),"!"),"@"),"@")
    val headParseResult = head.getWholeInputParser.parse(new StringReader(input))
    assert(headParseResult.successful)
    assertResult(expectation)(headParseResult.get)

    val secondParseResult = second.getWholeInputParser.parse(new StringReader(input))
    assert(secondParseResult.successful)
    assertResult(expectation)(secondParseResult.get)
  }

  test("Optional before seed") {
    lazy val expression: Self[Any] = new Lazy(expression) ~ "@" | optional_a ~ "#"
    val result = expression.getWholeInputParser.parse(aesReader)
    assert(result.successful, result.toString)
  }

  /**
   * This fails similarly to (a | ab) ~ bc.
   * The optional causes it to be something like:
   *   expression1 = "!" ~ expression2 ~ "s" | "e"
   *   expression2 = expression2 ~ "s" | "e"
   * The expression2 will parse an "s", even though expression1 still needs to parse "s"
  */
  test("Optional before recursive") {
    lazy val expression: Self[Any] = optional_a ~ expression ~ "@" | "#"
    val result = expression.getWholeInputParser.parse(aesReader)
    assert(result.successful, result.toString)
  }

  test("Recursive defaults") {
    lazy val recursive: Self[Any] = new Lazy(recursive) ~ "@" | "@"
    lazy val parser = "!" ~ recursive
    val input = "c"
    val expectation = ("!", "@")
    val result = parser.getWholeInputParser.parse(new StringReader(input))
    assertResult(expectation)(result.resultOption.get)
  }

  // a cycle of lazy parsers causes a stack overflow, since they have no cycle check, but with a sequence in between it just fails.
  test("only recursive with sequence indirection") {
    lazy val first: Self[Any] = new Lazy(first) ~ "!"
    val input = "aaa"
    val parseResult = first.getWholeInputParser.parse(new StringReader(input))
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("recursive with sequence indirection and default, " +
    "applies the default after failing the recursion") {
    lazy val first: Self[Any] = (new Lazy(first) ~ "!" | "!")
    val input = "notavailable"
    val parseResult = first.getWholeInputParser.parse(new StringReader(input))
    assert(!parseResult.successful)
    val expectation = Some("!") //Could have been ("yes","!") with different implementation
    assertResult(expectation)(parseResult.resultOption)
  }

  test("CheckCache inner vs outer detector regression") {
    val failBase = new Lazy(Fail(None, "bla", 1))
    lazy val expression: Self[Any] = failBase | wholeNumber
    lazy val greaterThan: Self[Any] = relationalPrecedence.map(x => x.toString)
    lazy val withGreaterThan = expression | greaterThan
    lazy val lessThan: Self[Any] = relationalPrecedence ~ withGreaterThan
    lazy val relationalPrecedence = new Lazy(withGreaterThan | lessThan)

    val analysis = compile(relationalPrecedence)
    relationalPrecedence.getWholeInputParser.parse(new StringReader("3"))
  }

  test("fibonacci regression simplified (doesn't regress yet)") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"

    val variable = new Lazy(parseIdentifier.map(x => x), "variable")
    lazy val lastExpression = new Lazy(Fail(None, "last", 1) | variable | call, "lastExpression")
    lazy val relationalInner: Self[Any] = new Lazy(lastExpression, "expression")
    lazy val equality: Self[Any] = new Lazy(relational.map(x => x) ~ literalOrKeyword("==") ~ relationalInner.map(x => x), "equality")
    lazy val relational: Self[Any] = new Lazy(relationalInner | equality, "relational")
    lazy val assignmentTarget: Self[Any] = new Lazy(Fail(None, "fail", 1), "assignmentTarget")
    lazy val simpleAssignment: Self[Any] = new Lazy(assignmentTarget.map(x => x) ~ literalOrKeyword("=") ~ expression.map(x => x), "equality")
    lazy val assignment: Self[Any] = new Lazy(relational | simpleAssignment, "assignemnt")
    lazy val memberSelector = new Lazy(expression.map(x => x) ~ literalOrKeyword(".") ~ parseIdentifier.map(x => x), "member selector")
    lazy val callCallee = new Lazy(variable | memberSelector, "callCallee")
    lazy val call = new Lazy(callCallee.map(x => x) ~ "(" ~ expression.manySeparated(",", "parameter").map(x => x) ~ ")", "call")
    //val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++ JavaLanguage.fields ++ JavaLanguage.method))

    lazy val expression: Self[Any] = assignment | memberSelector
    val result = expression.getWholeInputParser.parse(new StringReader(input))
    assert(result.successful)
  }

  def attempts(steps: Int): () => Boolean = {
    var stepsTaken = 0
    () => {
      stepsTaken += 1
      stepsTaken >= steps
    }
  }
}
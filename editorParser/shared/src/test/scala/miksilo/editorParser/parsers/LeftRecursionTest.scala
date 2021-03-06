package miksilo.editorParser.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import miksilo.editorParser.parsers.strings.{CommonParserWriter, NoStateParserWriter}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.concurrent.TimeLimits

class LeftRecursionTest extends AnyFunSuite with CommonParserWriter
  with NoStateParserWriter
  with LeftRecursiveCorrectingParserWriter
  with TimeLimits
{

  val optional_a: SequenceParserExtensions[Any] = Literal("!").*
  val optionalCopy: SequenceParserExtensions[Any] = Literal("!").*
  def aesReader = "!#@"

  test("left recursion with lazy indirection") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "!" | "!"

    val input = "!!!"
    val parseResult = head.getWholeInputParser().parse(input)
    assert(parseResult.successful)
    val expectation = (("!","!"),"!")
    assertResult(expectation)(parseResult.get)
  }

  test("handles recursion in complicated graph structures") {
    lazy val leftMayNotCache = leftRec ~ "@"
    lazy val leftRec = leftPath.map(x => x)
    lazy val leftPath: Parser[Any] = new Lazy(leftMayNotCache | leftRec ~ "!" | "@")

    val input = "@@@"
    val leftParseResult = leftPath.getWholeInputParser().parse(input)
    assert(leftParseResult.successful)
    val expectation = (("@","@"),"@")
    assertResult(expectation)(leftParseResult.get)

    lazy val rightMayNotCache = rightRec ~ "@"
    lazy val rightRec = rightPath.map(x => x)
    lazy val rightPath: Parser[Any] = new Lazy(rightRec ~ "!" | rightMayNotCache | "@")
    val rightParseResult = rightPath.getWholeInputParser().parse(input)
    assertResult(leftParseResult)(rightParseResult)
  }

  //[head] second b second b second head a second head a second c
  /*
  De second initialResults is netjes: c | recurse(second) | recursive(head)
  Als die gegrown worden dan krijgen we: c | grow(c) -> ded | grow(recursive(head) -> recursive' | recursive(head)
  Voor head initialResults krijg ik twee paar results voor beide second recursions, en dan per paar..
   */

  test("left recursion inside left recursion") {
    lazy val head: Parser[Any] = second ~ "!" | second
    lazy val second: Parser[Any] = new Lazy(second, "secondRef") ~ "@" | head | "#"

    val input = "#!!@@"
    val expectation = (((("#","!"),"!"),"@"),"@")
    val headParseResult = head.getWholeInputParser().parse(input)
    assert(headParseResult.successful)
    assertResult(expectation)(headParseResult.get)

    val secondParseResult = second.getWholeInputParser().parse(input)
    assert(secondParseResult.successful)
    assertResult(expectation)(secondParseResult.get)
  }

  test("Optional before seed") {
    lazy val expression: Parser[Any] = new Lazy(expression) ~ "@" | optional_a ~ "#"
    val result = expression.getWholeInputParser().parse(aesReader)
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
    lazy val expression: Parser[Any] = optional_a ~ expression ~ "@" | "#"
    val result = expression.getWholeInputParser().parse(aesReader)
    assert(result.successful, result.toString)
  }

  test("Recursive defaults") {
    lazy val recursive: Parser[Any] = new Lazy(recursive) ~ "@" | "@"
    lazy val parser = "!" ~ recursive
    val input = "c"
    val expectation = ("!", "@")
    val result = parser.getWholeInputParser().parse(input)
    assertResult(expectation)(result.resultOption.get)
  }

  // a cycle of lazy parsers causes a stack overflow, since they have no cycle check, but with a sequence in between it just fails.
  test("only recursive with sequence indirection") {
    lazy val first: Parser[Any] = new Lazy(first) ~ "!"
    val input = "aaa"
    val parseResult = first.getWholeInputParser().parse(input)
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("recursive with sequence indirection and default, " +
    "applies the default after failing the recursion") {
    lazy val first: Parser[Any] = new Lazy(first) ~ "!" | "!"
    val input = "notavailable"
    val parseResult = first.getWholeInputParser().parse(input)
    assert(!parseResult.successful)
    val expectation = Some("!") //Could have been ("yes","!") with different implementation
    assertResult(expectation)(parseResult.resultOption)
  }

  test("CheckCache inner vs outer detector regression") {
    val failBase = new Lazy(Fail(None, "bla", 1))
    lazy val expression: Parser[Any] = failBase | wholeNumber
    lazy val greaterThan: Parser[Any] = relationalPrecedence.map(x => x.toString)
    lazy val withGreaterThan = expression | greaterThan
    lazy val lessThan: Parser[Any] = relationalPrecedence ~ withGreaterThan
    lazy val relationalPrecedence = new Lazy(withGreaterThan | lessThan)

    val analysis = compile(relationalPrecedence)
    relationalPrecedence.getWholeInputParser().parse("3")
  }

  test("fibonacci regression simplified (doesn't regress yet)") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"

    val variable = new Lazy(parseIdentifier.map(x => x), "variable")
    lazy val lastExpression = new Lazy(Fail(None, "last", 1) | variable | call, "lastExpression")
    lazy val relationalInner: Parser[Any] = new Lazy(lastExpression, "expression")
    lazy val equality: Parser[Any] = new Lazy(relational.map(x => x) ~ literalOrKeyword("==") ~ relationalInner.map(x => x), "equality")
    lazy val relational: Parser[Any] = new Lazy(relationalInner | equality, "relational")
    lazy val assignmentTarget: Parser[Any] = new Lazy(Fail(None, "fail", 1), "assignmentTarget")
    lazy val simpleAssignment: Parser[Any] = new Lazy(assignmentTarget.map(x => x) ~ literalOrKeyword("=") ~ expression.map(x => x), "equality")
    lazy val assignment: Parser[Any] = new Lazy(relational | simpleAssignment, "assignment")
    lazy val memberSelector = new Lazy(expression.map(x => x) ~ literalOrKeyword(".") ~ parseIdentifier.map(x => x), "member selector")
    lazy val callCallee = new Lazy(variable | memberSelector, "callCallee")
    lazy val call = new Lazy(callCallee.map(x => x) ~ "(" ~ expression.manySeparated(",", "parameter").map(x => x) ~ ")", "call")
    //val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++ JavaLanguage.fields ++ JavaLanguage.method))

    lazy val expression: Parser[Any] = assignment | memberSelector
    val result = expression.getWholeInputParser().parse(input)
    assert(result.successful)
  }

  test("deep left recursion") {
    val input = new String(Array.fill(1000)('a'))
    lazy val parser: Parser[Any] = new Lazy(parser ~ literal("a") | literal("a"))
    val result = parser.getWholeInputParser().parse(input)
    assert(result.errors.isEmpty)
  }

  ignore("deep right recursion") {
    val input = new String(Array.fill(1000)('a'))
    lazy val parser: Parser[Any] = new Lazy(literal("a") ~ parser | literal("a"))
    val result = parser.getWholeInputParser().parse(input)
    assert(result.errors.isEmpty)
  }

  def attempts(steps: Int): () => Boolean = {
    var stepsTaken = 0
    () => {
      stepsTaken += 1
      stepsTaken >= steps
    }
  }
}
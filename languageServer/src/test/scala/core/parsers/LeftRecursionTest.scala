package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import deltas.ClearPhases
import deltas.expression.relational.{EqualsComparisonDelta, RelationalPrecedenceDelta}
import deltas.expression.{ExpressionDelta, PostFixIncrementDelta, VariableDelta}
import deltas.javac.classes.SelectFieldDelta
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.{CallDelta, CallMemberDelta}
import deltas.javac.{CallVariableDelta, ExpressionAsRoot, JavaLanguage}
import deltas.statement.assignment.{AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import org.scalatest.FunSuite
import org.scalatest.concurrent.{TimeLimitedTests, TimeLimits}
import org.scalatest.time.{Millis, Span}
import util.{LanguageTest, TestLanguageBuilder}

class LeftRecursionTest extends FunSuite with CommonStringReaderParser
  with LeftRecursiveCorrectingParserWriter
  with TimeLimits
  //with TimeLimitedTests
{

  //val timeLimit = Span(200, Millis)

  val optional_a: EditorParserExtensions[Any] =  literal("a").*
  val optionalCopy: EditorParserExtensions[Any] = literal("a").*
  def aesReader = new StringReader("aes")

  test("left recursion with lazy indirection") {
    lazy val head: Self[Any] = new Lazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(parseResult.successful)
    val expectation = (("a","a"),"a")
    assertResult(expectation)(parseResult.get)
  }

  test("handles recursion in complicated graph structures") {
    lazy val leftMayNotCache = leftRec ~ "b"
    lazy val leftRec = leftPath.map(x => x)
    lazy val leftPath: Self[Any] = new Lazy(leftMayNotCache | leftRec ~ "a" | "b")

    val input = "bbb"
    val leftParseResult = leftPath.parseWholeInput(new StringReader(input))
    assert(leftParseResult.successful)
    val expectation = (("b","b"),"b")
    assertResult(expectation)(leftParseResult.get)

    lazy val rightMayNotCache = rightRec ~ "b"
    lazy val rightRec = rightPath.map(x => x)
    lazy val rightPath: Self[Any] = new Lazy(rightRec ~ "a" | rightMayNotCache | "b")
    val rightParseResult = rightPath.parseWholeInput(new StringReader(input))
    assertResult(leftParseResult)(rightParseResult)
  }

  //[head] second b second b second head a second head a second c
  /*
  De second initialResults is netjes: c | recurse(second) | recursive(head)
  Als die gegrown worden dan krijgen we: c | grow(c) -> ded | grow(recursive(head) -> recursive' | recursive(head)
  Voor head initialResults krijg ik twee paar results voor beide second recursions, en dan per paar..
   */
  test("left recursion inside left recursion") {
    lazy val head: Self[Any] = second ~ "a" | second
    lazy val second: Self[Any] = new Lazy(second, "secondRef") ~ "b" | head | "c"

    val input = "caabb"
    val expectation = (((("c","a"),"a"),"b"),"b")
    val headParseResult = head.parseWholeInput(new StringReader(input), attempts(1))
    assert(headParseResult.successful)
    assertResult(expectation)(headParseResult.get)

    val secondParseResult = second.parseWholeInput(new StringReader(input))
    assert(secondParseResult.successful)
    assertResult(expectation)(secondParseResult.get)
  }

  test("Optional before seed") {
    lazy val expression: Self[Any] = new Lazy(expression) ~ "s" | optional_a ~ "e"
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
  test("Optional before recursive") {
    lazy val expression: Self[Any] = optional_a ~ expression ~ "s" | "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  test("Recursive defaults") {
    lazy val recursive: Self[Any] = new Lazy(recursive) ~ "b" | "b"
    lazy val parser = "a" ~ recursive
    val input = "c"
    val expectation = ("a", "b")
    val result = parser.parseWholeInput(new StringReader(input))
    assertResult(expectation)(result.resultOption.get)
  }

  // a cycle of lazy parsers causes a stack overflow, since they have no cycle check, but with a sequence in between it just fails.
  test("only recursive with sequence indirection") {
    lazy val first: Self[Any] = new Lazy(first) ~ "a"
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("only recursive with sequence indirection and default, " +
    "does not apply the default after failing the recursion") {
    lazy val first: Self[Any] = (new Lazy(first) ~ "a").withDefault("yes", "a's")
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val expectation = None
    assertResult(expectation)(parseResult.resultOption)
  }

  test("recursive with sequence indirection and default, " +
    "applies the default after failing the recursion") {
    lazy val first: Self[Any] = (new Lazy(first) ~ "a" | "a").withDefault("yes", "a's")
    val input = "notavailable"
    val parseResult = first.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val expectation = Some("yes") //Could have been ("yes","a") with different implementation
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
    relationalPrecedence.parseWholeInput(new StringReader("3"))
  }

  //Break this down into a normal parser.
  test("recursion detector caching regression") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases,
      TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      JavaLanguage.deltas))
    assert(utils.compile("2 + 1").diagnostics.isEmpty)
//    assert(utils.compile("/* Hello */ 2").diagnostics.isEmpty)
//    assert(utils.compile("/* Hello */ 2 + 1").diagnostics.isEmpty)
  }

  test("fibonacci regression simplified (doesn't regress yet)") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"

    val variable = new Lazy(identifier.map(x => x), "variable")
    lazy val lastExpression = new Lazy(Fail(None, "last", 1) | variable | call, "lastExpression")
    lazy val relationalInner: Self[Any] = new Lazy(lastExpression, "expression")
    lazy val equality: Self[Any] = new Lazy(relational.map(x => x) ~ literal("==") ~ relationalInner.map(x => x), "equality")
    lazy val relational: Self[Any] = new Lazy(relationalInner | equality, "relational")
    lazy val assignmentTarget: Self[Any] = new Lazy(Fail(None, "fail", 1), "assignmentTarget")
    lazy val simpleAssignment: Self[Any] = new Lazy(assignmentTarget.map(x => x) ~ literal("=") ~ expression.map(x => x), "equality")
    lazy val assignment: Self[Any] = new Lazy(relational | simpleAssignment, "assignemnt")
    lazy val memberSelector = new Lazy(expression.map(x => x) ~ literal(".") ~ identifier.map(x => x), "member selector")
    lazy val callCallee = new Lazy(variable | memberSelector, "callCallee")
    lazy val call = new Lazy(callCallee.map(x => x) ~ "(" ~ expression.manySeparated(",", "parameter").map(x => x) ~ ")", "call")
    //val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++ JavaLanguage.fields ++ JavaLanguage.method))

    lazy val expression: Self[Any] = assignment | memberSelector
    val result = expression.parseWholeInput(new StringReader(input))
    assert(result.successful)
  }

  //De outer ternary heeft een seed nodig. Die seed krijgt < als root, die meelift op een andere fixpoint, relational ofzo.
  //De seed wordt uiteindelijk iets waarbij 2?1:y de ternary wordt, maar daarvoor moet het gehaal wel in een assignment genest, zijn en die =<value> ontbreekt dan.
  test("moving ternary") {
    val input = """x<2?1:y"""
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(
      Seq(ClearPhases, ExpressionAsRoot) ++
      JavaLanguage.deltas))
    assert(utils.compile(input).diagnostics.isEmpty)
  }

  test("fibonacci regression") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"
    val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++
      Seq(CallMemberDelta, SelectFieldDelta, MemberSelectorDelta) ++ Seq(
      CallVariableDelta, CallDelta) ++ Seq(
      SimpleAssignmentDelta,
      AssignmentPrecedence, VariableDelta) ++ Seq(EqualsComparisonDelta,
      RelationalPrecedenceDelta, ExpressionDelta) ++ JavaLanguage.types
    ))

    val result = language.compile(input)
    assert(result.diagnostics.isEmpty)
  }

  test("postfix regression") {
    val input = """System.out.print(x++)""".stripMargin
    val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++
      Seq(AssignToVariable, PostFixIncrementDelta, CallMemberDelta, SelectFieldDelta, MemberSelectorDelta) ++ Seq(
      CallVariableDelta, CallDelta) ++ Seq(
      SimpleAssignmentDelta,
      AssignmentPrecedence, VariableDelta) ++ Seq(EqualsComparisonDelta,
      RelationalPrecedenceDelta, ExpressionDelta) ++ JavaLanguage.types
    ))
    val result = language.compile(input)
    assert(result.diagnostics.isEmpty)
  }

  test("postfix regression 2") {
    val input = """System.out.print(x++)""".stripMargin
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++
      JavaLanguage.deltas))
    assert(utils.compile(input).diagnostics.isEmpty)
  }

  def attempts(steps: Int): () => Boolean = {
    var stepsTaken = 0
    () => {
      stepsTaken += 1
      stepsTaken >= steps
    }
  }
}
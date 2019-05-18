package test.core.parsers

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.{Result, Self}
import core.bigrammar.grammars.{Keyword, Labelled}
import core.bigrammar.{BiGrammar, BiGrammarToParser, BiGrammarWriter, PrintBiGrammar, TestLanguageGrammarUtils}
import core.deltas.GrammarForAst
import core.parsers.CommonStringReaderParser
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import core.parsers.strings.CommonParserWriter
import core.smarts.SolveConstraintsDelta
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.logical.LogicalNotDelta
import deltas.{ClearPhases, HasNameDelta}
import deltas.expression.relational.{EqualsComparisonDelta, GreaterThanDelta, LessThanDelta, RelationalPrecedenceDelta}
import deltas.expression.{ExpressionDelta, IntLiteralDelta, LeftAssociativeBinaryOperatorDelta, ParenthesisInExpressionDelta, PostFixIncrementDelta, TernaryDelta, VariableDelta}
import deltas.javac.classes.{AssignToMemberDelta, FieldDeclarationDelta, FieldDeclarationWithInitializer, SelectFieldDelta}
import deltas.javac.expressions.equality.AddEqualityPrecedence
import deltas.javac.expressions.literals.{BooleanLiteralDelta, LongLiteralDelta, NullDelta}
import deltas.javac.methods.{AccessibilityFieldsDelta, ImplicitReturnAtEndOfMethod, MemberSelectorDelta, MethodDelta, ReturnExpressionDelta, ReturnVoidDelta}
import deltas.javac.methods.call.{CallDelta, CallMemberDelta}
import deltas.javac.{CallVariableDelta, ExpressionAsRoot, JavaLanguage, JavaToByteCodeLanguage}
import deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import langserver.types.Position
import org.scalatest.FunSuite
import util.{LanguageTest, TestLanguageBuilder}

import scala.collection.mutable

class LeftRecursionTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

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

  test("left recursion inside left recursion") {
    lazy val head: Self[Any] = second ~ "a" | second
    lazy val second: Self[Any] = new Lazy(second) ~ "b" | head | "c"

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

  //!! ALS IK CheckCache UITZET LUKT HET OPEENS WEL
  test("regression test from bigrammar Java") {
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
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      JavaToByteCodeLanguage.javaCompilerDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("fibonacci regression simplified") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"

    val variable = new Lazy(identifier, "variable")
    lazy val expression: Self[Any] = new Lazy(variable | memberSelector | call, "expression")
    lazy val memberSelector = new Lazy(expression ~ literal(".") ~ identifier, "member selector")
    lazy val call = new Lazy((variable | memberSelector) ~ "(" ~ expression.manySeparated(",", "parameter") ~ ")", "call")
    //val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, ExpressionAsRoot) ++ JavaLanguage.fields ++ JavaLanguage.method))
    val result = expression.parseWholeInput(new StringReader(input))
    assert(result.successful)
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
}
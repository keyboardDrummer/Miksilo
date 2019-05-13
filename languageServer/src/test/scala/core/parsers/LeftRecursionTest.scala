package test.core.parsers

import core.bigrammar.grammars.Labelled
import core.bigrammar.{BiGrammar, BiGrammarToParser, BiGrammarWriter, TestLanguageGrammarUtils}
import core.deltas.{Contract, DeltaWithGrammar}
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.Language
import core.parsers.CommonStringReaderParser
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import deltas.HasNameDelta
import deltas.expression.{ExpressionDelta, IntLiteralDelta}
import deltas.expression.relational.{GreaterThanDelta, LessThanDelta, RelationalPrecedenceDelta}
import deltas.javac.ExpressionAsRoot
import org.scalatest.FunSuite
import util.{LanguageTest, TestLanguageBuilder}

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

  test("relational direct") {
    lazy val expression: Self[Any] = new Lazy(wholeNumber)
    lazy val withGreaterThan = expression | greaterThan
    lazy val greaterThan: Self[Any] = withGreaterThan ~ ">" ~ expression
    lazy val relationalPrecedence = new Lazy(withGreaterThan | lessThan)
    lazy val lessThan: Self[Any] = relationalPrecedence ~ "<" ~ withGreaterThan

    expression.parseWholeInput(new StringReader("3<3"))
  }

  test("relational bigrammar") {
    import core.bigrammar.DefaultBiGrammarWriter._
    val innerExpression = new Labelled(ExpressionDelta.LastPrecedenceGrammar)
    val expression: Labelled = new Labelled(ExpressionDelta.FirstPrecedenceGrammar, innerExpression)
    expression.addAlternative(BiGrammarWriter.integer)
    val relationalPrecedence = new Labelled(RelationalPrecedenceDelta.Grammar, expression.inner)
    expression.inner = relationalPrecedence
    relationalPrecedence.addAlternative(relationalPrecedence ~ ">" ~ relationalPrecedence.inner)
    relationalPrecedence.addAlternative(relationalPrecedence ~ "<" ~ relationalPrecedence.inner)

    BiGrammarToParser.toParser(expression).parseWholeInput(new BiGrammarToParser.Reader("3<3"))
  }

  test("relational") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ExpressionAsRoot) ++
      Seq(LessThanDelta, GreaterThanDelta,
        RelationalPrecedenceDelta, IntLiteralDelta,
        HasNameDelta, ExpressionDelta)))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("3 < 3", grammarTransformer = ExpressionDelta.FirstPrecedenceGrammar)
  }
}

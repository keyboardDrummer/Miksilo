package deltas.javac

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.language.Language
import core.language.node.{Node, NodeField, NodeGrammar, NodeShape}
import deltas.PrettyPrint
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.{ExpressionDelta, IntLiteralDelta}
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import deltas.statement.{BlockDelta, StatementDelta}
import util.{LanguageTest, SourceUtils, TestLanguageBuilder}

import scala.reflect.io.Path

class JavaStyleCommentsTest
  extends LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta) ++ JavaToByteCodeLanguage.javaCompilerDeltas))
  with NodeGrammarWriter
{
  object ParentClass extends NodeShape
  object ChildClass extends NodeShape
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("comments with trivia inside node on tiny grammar") {
    val language = LanguageFromDeltas(Seq.empty)
    val grammars = language.grammars
    import grammars._

    val grammar: BiGrammar = "ParentStart" ~ identifier.as(ParentName) ~
      ("ChildStart" ~ identifier.as(ChildName) ~ "ChildEnd" asLabelledNode ChildClass).as(ParentChild) ~ "ParentEnd" asLabelledNode ParentClass
    grammars.root.inner = grammar
    SlashStarBlockCommentsDelta.transformGrammars(grammars, language)
    StoreTriviaDelta.transformGrammars(grammars, language)
    TriviaInsideNode.transformGrammars(grammars, language)

    val parsed = TestGrammarUtils.parse(
      """ParentStart/*b*/Remy/*c*/ChildStart Judith /*d*/ ChildEnd /*e*/ ParentEnd""".stripMargin, grammar)
    assert(parsed.successful, parsed.toString)
    val ast = parsed.get.asInstanceOf[Node]
    val printed = TestGrammarUtils.print(ast, grammar)
    val expected = """ParentStart/*b*/ Remy/*c*/ ChildStartJudith/*d*/ ChildEnd/*e*/ ParentEnd"""
    assertResult(expected)(printed)

    val slaveNode = ast(ParentChild).asInstanceOf[Node]
    val slaveGrammar = grammar.findAs(ParentChild).value.asInstanceOf[As].inner
    val slavePrinted = TestGrammarUtils.print(slaveNode, slaveGrammar)
    val slaveExpectation = """/*c*/ ChildStartJudith/*d*/ ChildEnd"""
    assertResult(slaveExpectation)(slavePrinted)
  }

  val testGrammar = TestLanguageGrammarUtils(this.language.deltas)

  test("BasicClass") {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, SlashStarBlockCommentsDelta.commentGrammar)
  }

  object ExpressionAsRoot extends DeltaWithGrammar
  {
    override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
      grammars.find(BodyGrammar).inner = grammars.find(ExpressionDelta.FirstPrecedenceGrammar)
    }

    override def description: String = ""

    override def dependencies: Set[Contract] = Set.empty
  }

  test("relational") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      JavaToByteCodeLanguage.javaCompilerDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("i < 3", grammarTransformer = ExpressionDelta.FirstPrecedenceGrammar)
  }

  test("addition") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      Seq(AdditionDelta, AdditivePrecedenceDelta, IntLiteralDelta, ExpressionDelta) ++
      ExtendedByteCode.allByteCodeDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition2") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      Seq(AdditionDelta, SubtractionDelta, AdditivePrecedenceDelta, IntLiteralDelta, ExpressionDelta) ++
      ExtendedByteCode.allByteCodeDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition3") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      Seq(SubtractionDelta, AdditionDelta, AdditivePrecedenceDelta, IntLiteralDelta, ExpressionDelta) ++
      ExtendedByteCode.allByteCodeDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition4") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, ExpressionAsRoot) ++
      JavaToByteCodeLanguage.javaCompilerDeltas))
    val grammarUtils = TestLanguageGrammarUtils(utils.language.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("block transformation") {
    val java = TestLanguageBuilder.buildWithParser(JavaToByteCodeLanguage.javaCompilerDeltas).buildLanguage
    val statementGrammar = java.grammars.find(StatementDelta.Grammar)
    statementGrammar.inner = new NodeGrammar("statement", ParentClass)
    val blockGrammar = java.grammars.find(BlockDelta.BlockGramar)
    val language = LanguageFromDeltas(Seq.empty)
    language.grammars.root.inner = blockGrammar
    TriviaInsideNode.transformGrammars(language.grammars, language)

    val expectedStatementGrammar: BiGrammar =
      new NodeGrammar(new WithTrivia("statement", language.grammars.trivia, false), ParentClass)

    import DefaultBiGrammarWriter._
    val expectedBlockGrammar = "{" %>
      new Labelled(StatementDelta.Grammar).manyVertical.as(BlockDelta.Statements).indent() %<
      new WithTrivia(BiGrammarWriter.stringToGrammar("}"), language.grammars.trivia, false) asNode(BlockDelta.Shape)
    assertResult(expectedBlockGrammar.toString)(blockGrammar.inner.toString) //TODO don't use toString
    assertResult(expectedStatementGrammar.toString)(statementGrammar.inner.toString) //TODO don't use toString
  }

  test("comparePrintResultWithoutComment") {
    val input = SourceUtils.getJavaTestFileContents("Whilee")
    val result = TestGrammarUtils.parseAndPrint(input, None, TestLanguageGrammarUtils.getGrammarUsingTransformer())
    assertResult(input)(result)
  }

  test("comparePrintResult") {
    val input = SourceUtils.getJavaTestFileContents("WhileeWithComment.java")
    val result = TestGrammarUtils.parseAndPrint(input, None, testGrammar.getGrammarUsingTransformer())
    assertResult(input)( result)
  }

  test("FullPipeline") {
    val inputDirectory = Path("")
    val output: String = compileAndRun("WhileeWithComment.java", inputDirectory)
    assertResult("3")( output)
  }

  test("comments are maintained in bytecode") {
    val byteCodeWithComments = Delta.spliceAndFilterTop(
      JavaToByteCodeLanguage.javaCompilerDeltas,
      ByteCodeLanguage.byteCodeDeltas,
      Seq(PrettyPrint(), TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta))
    val deltas = Seq(TriviaInsideNode, StoreTriviaDelta) ++ byteCodeWithComments
    val language = new LanguageTest(TestLanguageBuilder.buildWithParser(deltas))
    val result = language.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("FibonacciWithComments.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciWithCommentsByteCode.txt")
    assertResult(expectedResult)(result)
  }
}

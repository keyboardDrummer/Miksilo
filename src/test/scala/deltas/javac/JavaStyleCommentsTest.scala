package deltas.javac

import application.compilerBuilder.PresetsPanel
import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas._
import core.deltas.grammars.{BodyGrammar, LanguageGrammars}
import core.deltas.node.{Node, NodeField, NodeShape}
import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.expressions.additive.{AddAdditivePrecedence, AdditionDelta, SubtractionC}
import deltas.javac.expressions.literals.IntLiteralDelta
import deltas.javac.statements.{BlockDelta, StatementSkeleton}
import deltas.javac.trivia.{JavaStyleCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import util.{SourceUtils, TestLanguageBuilder, TestUtils}

import scala.reflect.io.Path

class JavaStyleCommentsTest
  extends TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta) ++ JavaCompilerDeltas.javaCompilerDeltas))
  with NodeGrammarWriter
{
  object ParentClass extends NodeShape
  object ChildClass extends NodeShape
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("comments with trivia inside node on tiny grammar") {
    val language = new Language(Seq.empty)
    val grammars = language.grammars
    import grammars._

    val grammar: BiGrammar = "ParentStart" ~ identifier.as(ParentName) ~
      ("ChildStart" ~ identifier.as(ChildName) ~ "ChildEnd" asLabelledNode ChildClass).as(ParentChild) ~ "ParentEnd" asLabelledNode ParentClass
    grammars.root.inner = grammar
    JavaStyleCommentsDelta.transformGrammars(grammars, language)
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

  val testGrammar = TestCompilerGrammarUtils(this.compiler.deltas)

  test("BasicClass") {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, JavaStyleCommentsDelta.getCommentGrammar)
  }

  object ExpressionAsRoot extends DeltaWithGrammar
  {
    override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
      grammars.find(BodyGrammar).inner = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    }

    override def description: String = ""
  }

  test("relational") {
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(JavaStyleCommentsDelta, ExpressionAsRoot) ++
      JavaCompilerDeltas.javaCompilerDeltas))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("i < 3", grammarTransformer = ExpressionSkeleton.ExpressionGrammar)
  }
//Deze doet het nu opeens wel. Komt dat door de wijzigingen in JSCommentsC? Of door de missende substraction? Reproduceer het probleem opnieuw
  test("addition") {
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta, ExpressionAsRoot) ++
      Seq(AdditionDelta, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
      JavaCompilerDeltas.allByteCodeDeltas))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition2") {
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta, ExpressionAsRoot) ++
      Seq(AdditionDelta, SubtractionC, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
      JavaCompilerDeltas.allByteCodeDeltas))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition3") {
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta, ExpressionAsRoot) ++
      Seq(SubtractionC, AdditionDelta, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
    JavaCompilerDeltas.allByteCodeDeltas))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition4") {
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta, ExpressionAsRoot) ++
      JavaCompilerDeltas.javaCompilerDeltas))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("block transformation") {
    val java = TestLanguageBuilder.build(JavaCompilerDeltas.javaCompilerDeltas).buildLanguage
    val statementGrammar = java.grammars.find(StatementSkeleton.StatementGrammar)
    statementGrammar.inner = new NodeGrammar("statement", ParentClass)
    val blockGrammar = java.grammars.find(BlockDelta.Grammar)
    val language = new Language(Seq.empty)
    language.grammars.root.inner = blockGrammar
    TriviaInsideNode.transformGrammars(language.grammars, language)

    val expectedStatementGrammar: BiGrammar = new NodeGrammar(new WithTrivia("statement", language.grammars.trivia, false), ParentClass)

    val expectedBlockGrammar = new TopBottom(new TopBottom("{", new ManyVertical(new Labelled(StatementSkeleton.StatementGrammar)).indent()).ignoreLeft,
      new WithTrivia("}", language.grammars.trivia, false)).ignoreRight
    assertResult(expectedBlockGrammar.toString)(blockGrammar.inner.toString) //TODO don't use toString
    assertResult(expectedStatementGrammar.toString)(statementGrammar.inner.toString) //TODO don't use toString
  }

  test("comparePrintResultWithoutComment") {
    val input = SourceUtils.getJavaTestFileContents("Whilee")
    val result = TestGrammarUtils.parseAndPrint(input, None, TestCompilerGrammarUtils.getGrammarUsingTransformer())
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
    val initialCompiler = TestLanguageBuilder.build(PresetsPanel.getJavaCompilerParticles)
    val utils = new TestUtils(TestLanguageBuilder.build(Seq(TriviaInsideNode, StoreTriviaDelta) ++
      initialCompiler.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeDeltas, Seq(TriviaInsideNode, StoreTriviaDelta, JavaStyleCommentsDelta))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("FibonacciWithComments.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciWithCommentsByteCode.txt")
    assertResult(expectedResult)(result)
  }
}

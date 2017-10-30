package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.bigrammar.{As, BiGrammar, TestCompilerGrammarUtils, TestGrammarUtils}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{Node, NodeClass, NodeField}
import core.particles.{DeltaWithGrammar, Language, NodeGrammarWriter}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionDelta, SubtractionC}
import transformations.javac.expressions.literals.IntLiteralDelta
import util.{CompilerBuilder, SourceUtils, TestUtils}

import scala.reflect.io.Path

class TestComments
  extends TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC) ++ JavaCompilerDeltas.javaCompilerTransformations))
  with NodeGrammarWriter
{
  object ParentClass extends NodeClass
  object ChildClass extends NodeClass
  object ParentName extends NodeField
  object ParentChild extends NodeField
  object ChildName extends NodeField

  test("test injection") {
    val grammar: BiGrammar = "{" ~ identifier.as(ParentName) ~~
      ("_" ~ identifier.as(ChildName) ~ "_" asNode ChildClass).as(ParentChild) ~ "}" asNode ParentClass
    val grammars = new GrammarCatalogue
    grammars.create(ProgramGrammar, grammar)
    JavaStyleCommentsC.transformGrammars(grammars, new Language)

    val parsed = TestGrammarUtils.parse(
      """ /*a*/ { /*b*/ remy /*c*/ _ judith /*d*/ _ /*e*/ } """.stripMargin, grammar)
    assert(parsed.successful, parsed.toString)
    val ast = parsed.get.asInstanceOf[Node]
    val printed = TestGrammarUtils.print(ast, grammar)
    val expected = "/*a*/ {/*b*/ remy /*c*/ _judith/*d*/ _/*e*/ }"
    assertResult(expected)(printed)

    val slaveNode = ast(ParentChild).asInstanceOf[Node]
    val slaveGrammar = grammar.findAs(ParentChild).get.asInstanceOf[As].inner
    val slavePrinted = TestGrammarUtils.print(slaveNode, slaveGrammar)
    val slaveExpectation = "/*c*/ _judith/*d*/ _"
    assertResult(slaveExpectation)(slavePrinted)
  }

  val testGrammar = TestCompilerGrammarUtils(this.compiler.deltas)

  test("BasicClass") {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, JavaStyleCommentsC.getCommentGrammar)
  }

  object ExpressionAsRoot extends DeltaWithGrammar
  {
    override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
      grammars.create(ProgramGrammar, grammars.find(ExpressionSkeleton.ExpressionGrammar))
    }

    override def description: String = ""
  }

  test("relational") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      JavaCompilerDeltas.javaCompilerTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("i < 3", grammarTransformer = ExpressionSkeleton.ExpressionGrammar)
  }
//Deze doet het nu opeens wel. Komt dat door de wijzigingen in JSCommentsC? Of door de missende substraction? Reproduceer het probleem opnieuw
  test("addition") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(AdditionDelta, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
      JavaCompilerDeltas.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition2") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(AdditionDelta, SubtractionC, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
      JavaCompilerDeltas.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition3") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(SubtractionC, AdditionDelta, AddAdditivePrecedence, IntLiteralDelta, ExpressionSkeleton) ++
    JavaCompilerDeltas.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition4") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      JavaCompilerDeltas.javaCompilerTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("comparePrintResultWithoutComment") {
    val input = SourceUtils.getJavaTestFileContents("Whilee")
    val result = TestGrammarUtils.parseAndPrint(input, None, TestCompilerGrammarUtils.getGrammarUsingTransformer())
    assertResult(input)( result)
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
    val initialCompiler = CompilerBuilder.build(PresetsPanel.getJavaCompilerParticles)
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC) ++ initialCompiler.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeTransformations, Seq(JavaStyleCommentsC))))
    //val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC) ++ initialCompiler.deltas)) //++ initialCompiler.spliceBeforeTransformations(JavaCompilerDeltas.byteCodeTransformations, Seq(JavaStyleCommentsC))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("FibonacciWithComments.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciWithCommentsByteCode.txt")
    assertResult(expectedResult)(result)
  }
}

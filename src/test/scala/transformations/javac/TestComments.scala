package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.bigrammar.{TestCompilerGrammarUtils, TestGrammarUtils}
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.{DeltaWithGrammar, Language}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionC, SubtractionC}
import transformations.javac.expressions.literals.IntLiteralC
import util.{CompilerBuilder, SourceUtils, TestUtils}

import scala.reflect.io.Path

class TestComments extends TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC) ++ JavaCompiler.javaCompilerTransformations)) {

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
      JavaCompiler.javaCompilerTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("i < 3", grammarTransformer = ExpressionSkeleton.ExpressionGrammar)
  }
//Deze doet het nu opeens wel. Komt dat door de wijzigingen in JSCommentsC? Of door de missende substraction? Reproduceer het probleem opnieuw
  test("addition") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(AdditionC, AddAdditivePrecedence, IntLiteralC, ExpressionSkeleton) ++
      JavaCompiler.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition2") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(AdditionC, SubtractionC, AddAdditivePrecedence, IntLiteralC, ExpressionSkeleton) ++
      JavaCompiler.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition3") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      Seq(SubtractionC, AdditionC, AddAdditivePrecedence, IntLiteralC, ExpressionSkeleton) ++
    JavaCompiler.allByteCodeTransformations))
    val grammarUtils = TestCompilerGrammarUtils(utils.compiler.deltas)

    grammarUtils.compareInputWithPrint("2 + 1")
    grammarUtils.compareInputWithPrint("/* Hello */ 2")
    grammarUtils.compareInputWithPrint("/* Hello */ 2 + 1")
  }

  test("addition4") {
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC, ExpressionAsRoot) ++
      JavaCompiler.javaCompilerTransformations))
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
    val utils = new TestUtils(CompilerBuilder.build(Seq(JavaStyleCommentsC) ++ initialCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(JavaStyleCommentsC))))
    val result = utils.compileAndPrettyPrint(SourceUtils.getJavaTestFileContents("FibonacciWithComments.java"))
    val expectedResult = SourceUtils.getTestFileContents("FibonacciWithCommentsByteCode.txt")
    assertResult(expectedResult)(result)
  }
}

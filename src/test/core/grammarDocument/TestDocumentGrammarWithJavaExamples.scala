package core.grammarDocument

import application.compilerCockpit._
import core.modularProgram.PieceCombiner
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.{GrammarTransformation, Injector}
import core.transformation.{MetaObject, TransformationState}
import org.junit.{Assert, Test}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions.TernaryC
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MethodC}
import transformations.javac.statements.BlockC
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, JavaCompiler}
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples {
  val lineSeparator = System.lineSeparator()

  @Test
  def testSimpleForLoop() {
    val testFileContent = TestUtils.getJavaTestFile("SimpleForLoop", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testWhile() {
    val testFileContent = TestUtils.getJavaTestFile("whilee", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testFibonacci() {
    val testFileContent = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testTernary() {
    val input = "1 ? 2 : 3"
    TestGrammarUtils.compareInputWithPrint(input, None, TernaryC.TernaryExpressionGrammar)
  }

  @Test
  def testFibonacciMainMethod() {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestGrammarUtils.compareInputWithPrint(input, None, MethodC.MethodGrammar)
  }

  @Test
  def testBlock() {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }

  @Test
  def testPrintAfterImplicitAddition() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getJavaTestFile("ExplicitFibonacci", Path("")).slurp()

    val implicits = Seq[Injector](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, ImplicitReturnAtEndOfMethod)
    val newTransformations = Seq(new ParseFromFunction(() => input)) ++ implicits ++ Seq(PrettyPrint) ++
      JavaCompiler.spliceAfterTransformations(implicits, Seq(PrettyPrint))

    val state = new TransformationState
    PieceCombiner.combineAndExecute(state, newTransformations.reverse)
    val output = OutputOption.getOutput(state).get

    Assert.assertEquals(expectation, output)
  }

  @Test
  def testPrettyPrintByteCode() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()

    val newTransformations = Seq(new ParseFromFunction(() => input)) ++
      JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(PrettyPrint))

    val state = new TransformationState
    PieceCombiner.combineAndExecute(state, newTransformations.reverse)
    val output = OutputOption.getOutput(state).get

    Assert.assertEquals(expectation, output)
  }


  @Test
  def testPrettyPrintAndParseByteCode() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()

    val byteCodeTransformations: Seq[GrammarTransformation] = JavaCompiler.byteCodeTransformations
    val prettyPrintTransformations = Seq(new ParseFromFunction(() => input)) ++
      JavaCompiler.spliceBeforeTransformations(byteCodeTransformations, Seq(PrettyPrint))

    val firstState = new TransformationState
    PieceCombiner.combineAndExecute(firstState, prettyPrintTransformations.reverse)
    val byteCode = OutputOption.getOutput(firstState).get

    val secondState = new TransformationState
    val parseTransformations = Seq(new ParseFromFunction(() => byteCode)) ++ byteCodeTransformations ++ Seq(EmitByteCode)
    PieceCombiner.combineAndExecute(secondState, parseTransformations.reverse)
    val output = OutputOption.getOutput(secondState).get
    Assert.assertEquals(8, output)
  }

  @Test
  def parseByteCode() {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val secondState = new TransformationState
    val parseTransformations = Seq(new ParseFromFunction(() => input)) ++ JavaCompiler.byteCodeTransformations ++ Seq(CompileAndRun)
    PieceCombiner.combineAndExecute(secondState, parseTransformations.reverse)
    val output = OutputOption.getOutput(secondState).get
    Assert.assertEquals(8, output)
  }

  @Test
  def testPrettyPrintMethodInfo(): Unit = {
    val methodInfo = ByteCodeSkeleton.methodInfo(1,2,Seq.empty[MetaObject],Set.empty[ByteCodeSkeleton.MethodAccessFlag])
    val grammars = new GrammarCatalogue
    ByteCodeSkeleton.transformGrammars(grammars)
    BiGrammarToDocument.toDocument(methodInfo, grammars.find(ByteCodeSkeleton.MethodInfoGrammar))
  }

  @Test
  def testPrettyAddressLoad(): Unit = {
    val grammar = LoadAddressC.getGrammarForThisInstruction
    val addressLoad = LoadAddressC.addressLoad(0)
    BiGrammarToDocument.toDocument(addressLoad, grammar)
  }
}
